package kvstore

import akka.actor.{ Props, ActorRef, Actor }
import kvstore.Arbiter._
import scala.concurrent.duration._
import akka.actor.PoisonPill
import scala.language.postfixOps

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */
  arbiter ! Join
  System.out.println(f"Replica $self created")
  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]

  var persistence = context.actorOf(persistenceProps)

  var lastSeq: Long = -1

  var pending = Map.empty[Long, (ActorRef, Long, Persist, Object)]

  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  context.system.scheduler.schedule(100 millis, 100 millis) {
    val now = System.currentTimeMillis();
    pending.foreach({
      case (id, (actor, timestamp, persist, reply)) =>
        if (now - timestamp > 1000) {
          actor ! OperationFailed(id)
          pending = pending - id
        } else {
          persistence ! persist
        }
    })
  }

  val leader: Receive = {
    case Insert(key, value, id) =>
      kv = kv.updated(key, value)
      val persist = Persist(key, Some(value), id)
      pending = pending.updated(id, (sender, System.currentTimeMillis(), persist, OperationAck(id)))
      replicators.foreach(_ ! Replicate(key, Some(value), id))
    case Remove(key, id) =>
      kv = kv - key
      val persist = Persist(key, None, id)
      pending = pending.updated(id, (sender, System.currentTimeMillis(), persist, OperationAck(id)))
      replicators.foreach(_ ! Replicate(key, None, id))
    case Persisted(key, id) =>
      pending.get(id) match {
        case Some((actor, timestamp, persist, reply)) =>
          pending = pending - id
          actor ! reply
        case None =>
          System.err.println(f"operation $id is not pending")
      }
    case Get(key, id) =>
      sender ! GetResult(key, kv.get(key), id)
    case Replicas(replicas) =>
      val toRemove = secondaries.keySet.diff(replicas)
      val toAdd = replicas.diff(secondaries.keySet)

      System.out.println(f"Replicas : toRemove $toRemove, toAdd $toAdd")

      toRemove.foreach(replica => {
        secondaries.get(replica) match {
          case Some(replicator) =>
            replicator ! PoisonPill
            secondaries = secondaries - replica
            replicators = replicators - replicator
            System.out.println(f"removed $replica and stopped its replicator $replicator")
          case None =>
            System.err.println(f"replicator of $replica not found")
        }
      })

      toAdd.foreach(replica => {
        val replicator = context.actorOf(Replicator.props(replica))
        secondaries = secondaries.updated(replica, replicator)
        replicators = replicators + replicator
        System.out.println(f"added replicator $replicator to replica $replica")
      })
  }

  val replica: Receive = {
    case Replicate(key, valueOption, id) =>
      valueOption match {
        case Some(value) =>
          kv = kv.updated(key, value)
        case None =>
          kv = kv - key
      }
      val persist = Persist(key, valueOption, id)
      pending = pending.updated(id, (sender, System.currentTimeMillis(), persist, Replicated(key, id)))
    case Snapshot(key, valueOption, seq) =>
      if (seq == lastSeq + 1) {
        valueOption match {
          case Some(value) =>
            kv = kv.updated(key, value)
          case None =>
            kv = kv - key
        }
        lastSeq = seq
      }
      if (seq <= lastSeq) {
        val persist = Persist(key, valueOption, -seq)
        pending = pending.updated(-seq, (sender, System.currentTimeMillis(), persist, SnapshotAck(key, seq)))
      }
    case Persisted(key, id) =>
      pending.get(id) match {
        case Some((actor, timestamp, persist, reply)) =>
          pending = pending - id
          actor ! reply
        case None =>
          System.err.println(f"operation $id is not pending")
      }
    case Get(key, id) =>
      sender ! GetResult(key, kv.get(key), id)
  }

}

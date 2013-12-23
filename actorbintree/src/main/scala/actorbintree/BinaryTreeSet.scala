/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Request to GC that migrates all children nodes to a new root node */
  case class GCRequest(root: ActorRef)

  /** Response that GC has completed and all children has migrated to the new root node */
  case object GCFinished

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(-1, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  def fake(op: Operation) =
    op match {
      case Insert(req, id, el) => Insert(self, id, el)
      case Remove(req, id, el) => Remove(self, id, el)
      case Contains(req, id, el) => Contains(self, id, el)
    }

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case op: Operation =>
      //println("appending " + op)
      //println("QUEUE: " + pendingQueue.map(_.id).take(10).mkString(",") + "...[" + pendingQueue.size + "]")
      if (pendingQueue.isEmpty) {
        //println("first-sending ( " + op.id + " ) " + op)
        //println("QUEUE: " + pendingQueue.map(_.id).take(10).mkString(",") + "...[" + pendingQueue.size + "]")
        root ! fake(op)
      }
      pendingQueue = pendingQueue :+ op
    case reply: OperationReply =>
      val (op, queue) = pendingQueue.dequeue
      pendingQueue = queue
      //println("replying " + reply)
      //println("QUEUE: " + pendingQueue.map(_.id).take(10).mkString(",") + "...[" + pendingQueue.size + "]")
      op.requester ! reply

      if (!pendingQueue.isEmpty) {
        val next = pendingQueue.front
        //println("queue-sending ( " + next.id + " ) " + next)
        //println("QUEUE: " + pendingQueue.map(_.id).take(10).mkString(",") + "...[" + pendingQueue.size + "]")
        root ! fake(next)
      }
    case GC =>
      //println("passing operation " + GC)
      val newRoot = createRoot
      root ! GCRequest(newRoot)
      context.become(garbageCollecting(newRoot))
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case op: Operation =>
      pendingQueue = pendingQueue :+ op
      //println("added pending operation while in GC:" + op)
    case GCFinished =>
      context.unbecome()
      root ! PoisonPill
      root = newRoot
      //println("GC finished: rescheduled " + pendingQueue.size + " pending operations : " + pendingQueue)
      if (!pendingQueue.isEmpty) {
        val next = pendingQueue.front
        //println("after-gc-sending ( " + next.id + " ) " + next)
        //println("QUEUE: " + pendingQueue.map(_.id).take(10).mkString(",") + "...[" + pendingQueue.size + "]")
        root ! fake(next)
      }
  }
}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  // remaining GCFinished replies to await
  var remaining = 0

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case Insert(requester, id, el) =>
      if (el == elem) {
        removed = false
        requester ! OperationFinished(id)
      } else {
        val pos = if (el > elem) Right else Left
        subtrees.get(pos) match {
          case Some(node) =>
            node ! Insert(requester, id, el)
          case None =>
            subtrees = subtrees.updated(pos, context.actorOf(props(el, initiallyRemoved = false)))
            requester ! OperationFinished(id)
        }
      }
    case Contains(requester, id, el) =>
      if (el == elem) {
        requester ! ContainsResult(id, !removed)
      } else {
        val pos = if (el > elem) Right else Left
        subtrees.get(pos) match {
          case Some(node) => node ! Contains(requester, id, el)
          case None => requester ! ContainsResult(id, false)
        }
      }
    case Remove(requester, id, el) =>
      if (el == elem) {
        removed = true
        requester ! OperationFinished(id)
      } else {
        val pos = if (el > elem) Right else Left
        subtrees.get(pos) match {
          case Some(node) => node ! Remove(requester, id, el)
          case None => requester ! OperationFinished(id)
        }
      }
    case GCRequest(root: ActorRef) =>
      if (!removed) {
        //println(elem + ": sending insert to the new root")
        root ! Insert(context.self, 0, elem)
      }
      remaining = 0
      subtrees.get(Left) match {
        case Some(node) =>
          //println(elem + ": sending GCRequest to left")
          node ! GCRequest(root)
          remaining += 1
        case None =>
      }
      subtrees.get(Right) match {
        case Some(node) =>
          //println(elem + ": sending GCRequest to right")
          node ! GCRequest(root)
          remaining += 1
        case None =>
      }
      if (remaining == 0) {
        sender ! GCFinished
      }
    case GCFinished =>
      remaining -= 1
      //println(elem + ": received GCFinished - remaining = " + remaining)
      if (remaining == 0) {
        context.parent ! GCFinished
      }


  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = ???

}

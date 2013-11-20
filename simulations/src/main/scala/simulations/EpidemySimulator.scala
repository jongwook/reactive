package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    // to complete: additional parameters of simulation
    val activateChosenFew = false
    val reduceMobility = false
    val activateAirTraffic = false

    val transmissibility: Double = 0.40
    val lethality: Double = 0.25
    val initialPrevalenceRate: Double = 0.01
    val chosenFewRatio: Double = if (activateChosenFew) 0.05 else 0.0
    val mobility: Int = if (reduceMobility) 10 else 5
    val airTrafficRatio: Double = if (activateAirTraffic) 0.01 else 0.0

  }

  import SimConfig._

  val persons: List[Person] = for {
    id <- (0 until population).toList
  } yield new Person(id)

  for ( i <- 0 until (population * initialPrevalenceRate).round.toInt ) {
    persons(randomBelow(population)).infected = true
  }

  for ( i <- 0 until (population * chosenFewRatio).round.toInt ) {
    persons(randomBelow(population)).chosen = true
  }

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false
    var chosen = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    def scheduleInfection {
      afterDelay(6) {
        sick = true
      }
      afterDelay(14) {
        if (random < lethality) {
          dead = true
        }
      }
      afterDelay(16) {
        if (!dead) {
          sick = false
          immune = true
        }
      }
      afterDelay(18) {
        if (!dead) {
          infected = false
          immune = false
        }
      }
    }

    def move() {
      // not moving if dead
      if (dead) return

      // schedule next move
      val delay = randomBelow(mobility) + 1
      afterDelay(delay) {

        // choose destination
        var destinations: List[(Int, Int)] = Nil

        if (random < airTrafficRatio) {
          // to a random room
          destinations = (randomBelow(roomRows), randomBelow(roomColumns)) :: Nil
        } else {
          // check four neighbors
          for {
            (dx, dy) <- (1, 0) :: (-1, 0) :: (0, 1) :: (0, -1) :: Nil
          } {
            val r = (row + dx + roomRows) % roomRows
            val c = (col + dy + roomColumns) % roomColumns
            // if there is visibly infectious (i.e. sick or dead) people in that room
            if (!persons.exists(p => p.row == r && p.col == c && (p.sick || p.dead))) {
              destinations = (r, c) :: destinations
            }
          }
        }

        // chose a random room among destinations, stay put if no destination found
        val destination = if (destinations.isEmpty) (row, col) else destinations(randomBelow(destinations.size))

        row = destination._1
        col = destination._2

        // if there is infectious people, get infected
        if (persons.exists(p => p.row == row && p.col == col && (p.infected || p.immune)) && random < transmissibility) {
          // only when not already infected or immune, and not chosen
          if (!infected && !immune && !chosen) {
            infected = true
            scheduleInfection
          }
        }

        move()
      }
    }


    //
    // to complete with simulation logic
    //
    afterDelay(0) {
      if (chosen) {
        infected = false
        sick = false
        immune = true
        dead = false
      }
      if (infected) {
        scheduleInfection
      }
      move()
    }
  }
}

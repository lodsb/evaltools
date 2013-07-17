package TreeQuencer

import collection.immutable.IndexedSeq

object RhythmComplexity {
  def rhythmicOddityPairsSequence(sequence: Seq[Boolean]): Int = {
    val n2 = sequence.length / 2

    (0 to n2 - 1).foldLeft(0)({
      (x, y) => if (sequence(y) && sequence(y + n2)) x + 1 else x;
    })
  }

  def rhythmicOddity(score: RhythmReconstruction.Score): Int = {
    if (score.length < 2) {
      0
    } else {
      val noTracks = score(0).length

      (0 to noTracks - 1) map {
        track => rhythmicOddityPairsSequence(score map {
          x => x(track)
        })
      } sum
    }
  }

  def wnbd(score: RhythmReconstruction.Score, meter: Int, track: Int): Double = {
       // map to score
    val sequence = score map {
      x => x(track)
    }
    wnbdSequence(sequence, meter)
  }

  def wnbdSequence(sequence: Seq[Boolean], meter: Int): Double = {

    val n = sequence.length

    val meters = (0 to n - 1) filter {
      _ % meter == 0
    }


    // calculate all distances for all onsets for each meter
    val distancesFromMeter = (sequence zipWithIndex) filter {
      x => x._1
    } map {
      x => meters map {
        beat => x._2 - beat
      }
    }

    // calculate membership of each onset to a meter
    val membershipMeter = (sequence zipWithIndex) filter {
      x => x._1
    } map {
      x => x._2 / meter
    }

    // calc length of "notes"
    val offsets = ((sequence zipWithIndex) filter {
      x => x._1
    } map {
      x => x._2
    })
    val noteLength = (offsets :+ sequence.length).sliding(2) map {
      x => x(1) - x(0)
    } toList


    val allData: Seq[((IndexedSeq[Int], Int), Int)] = (distancesFromMeter zip membershipMeter) zip noteLength

    val weights = allData map {
      x =>
        val mdist = x._1._1

        //val midx  = x._1._1._2
        val memberShip = x._1._2
        val memberShipE1 = memberShip + 1
        val memberShipE2 = memberShip + 2


        val noteLength = x._2

        val absDist0 = scala.math.abs(mdist(memberShip))

        val weight = if (absDist0 == 0) {
          0.0
                // e+1 exists
        } else if (memberShipE1 < mdist.length) {
          val absDist1 = scala.math.abs(mdist(memberShipE1))
          val Tx = scala.math.min(absDist0, absDist1) / meter.toDouble
          println("T(x) : " + Tx)
          // e+2 exists
          if (memberShipE2 < mdist.length) {
            val absDist2 = scala.math.abs(mdist(memberShipE2))

            // x start before meter e+1 ends before e+2
            if (noteLength > absDist1 && noteLength <= absDist2) {
              2.0 / Tx
            } else {
              1.0 / Tx
            }
          } else {
            1.0 / Tx
          }
        } else {

          0.0
        }

        weight
    }

    println("W: " + weights.sum)

    weights.sum / membershipMeter.length.toDouble
  }


}

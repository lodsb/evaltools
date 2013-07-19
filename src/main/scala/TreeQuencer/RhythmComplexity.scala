package TreeQuencer

import collection.immutable.IndexedSeq

object RhythmComplexity {
  def numEvents(score: RhythmReconstruction.Score): Seq[Double] = {
    if (score.length < 2) {
      Seq.fill(RhythmReconstruction.numInstruments()) {
        0.0
      }
    } else {
      val noTracks = score(0).length

      val n = (0 to noTracks - 1) map {
        track =>
          val seq = score map {
            x => x(track)
          }
          val noe = seq.foldLeft(0.0) {
            (x, y) => if (y) x + 1.0 else x
          }

          noe
      }

      n

    }
  }

  def rhythmicOddityPairsSequence(sequence: Seq[Boolean]): Double = {
    val n2 = sequence.length / 2

    (0 to n2 - 1).foldLeft(0)({
      (x, y) => if (sequence(y) && sequence(y + n2)) x + 1 else x;
    })
  }

  def rhythmicOddity(score: RhythmReconstruction.Score): Seq[Double] = {
    if (score.length < 2) {
      Seq.fill(RhythmReconstruction.numInstruments()) {
        0.0
      }
    } else {
      val noTracks = score(0).length

      (0 to noTracks - 1) map {
        track => rhythmicOddityPairsSequence(score map {
          x => x(track)
        })
      }
    }
  }

  def wnbd(score: RhythmReconstruction.Score, meter: Int): Seq[Double] = {
    // map to score

    var wnbd = Seq.fill(RhythmReconstruction.numInstruments()) {
      0.0
    }

    if (score.length >= meter) {
      wnbd = (0 to score(0).length - 1) map {
        track =>

          val sequence = score map {
            x => x(track)
          }

          wnbdSequence(sequence, meter)

      }
    }

    wnbd
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
      x => if (x.length >= 2) x(1) - x(0) else 1
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

    val divisor = if (membershipMeter.length > 0) membershipMeter.length.toDouble else 1.0

    val ret = weights.sum / divisor

    BigDecimal(ret).setScale(4, BigDecimal.RoundingMode.HALF_UP).toDouble
  }


}

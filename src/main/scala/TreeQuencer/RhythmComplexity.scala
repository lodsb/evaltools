package TreeQuencer

import RhythmReconstruction._

object RhythmComplexity {
  def rhythmicOddityPairsTrack(score: RhythmReconstruction.Score, track: Int) : Int = {
    val n2 = score.length/ 2

    (0 to n2-1).foldLeft(0)({ (x,y) => if(score(y)(track) && score(y+n2)(track)) x+1 else x; })
  }

  def rhytmicOddity(score: RhythmReconstruction.Score) : Int = {
    if(score.length < 2) {
      0
    } else {
      val noTracks = score(0).length

      (0 to noTracks-1) map {track => rhythmicOddityPairsTrack(score, track) } sum
    }
  }

  def wnbd(score: RhythmReconstruction.Score, meter: Int, track: Int ) : Double = {
    val n = score.length
    val meters = (0 to n-1) filter {_ % meter == 0 }

    val sequence = score map { x => x(track) }

    // calculate all distances for all onsets for each meter
    val distancesFromMeter = ( sequence zipWithIndex ) filter { x => x._1 } map { x => meters map { beat => x._2 - beat} }

    // calculate membership of each onset to a meter
    val membershipMeter = ( sequence zipWithIndex ) filter { x => x._1 } map { x => x._2 / meter }

    // calc length of "notes"
    val offsets = ((sequence zipWithIndex) filter {x => x._1} map {x => x._2})
    val noteLength  = (offsets :+ sequence.length).sliding(2) map {x => x(1) - x(0) } toList


    // look for the smallest absolute distance from a beat .. for all onsets
    val minimalDistances = distancesFromMeter map {x => (x, (( x zipWithIndex ) map { y => ( scala.math.abs(y._1), y._2 ) }).min._2 ) }

    val weights = minimalDistances zip (membershipMeter zip noteLength) map { x =>
      val mdist = x._1._1
      val midx  = x._1._2
      val memberShip = x._2._1
      val noteLength = x._2._2

      val absDistMeter = scala.math.abs(mdist(midx)) / meter

      val weight =  if (mdist == 0) {
                      0.0
                    // x start before meter e+1 ends before e+2
                    } else if (mdist == -1) {
                      2*absDistMeter
                    } else if (noteLength > mdist) {
                      2*absDistMeter
                    }





    }





    0.0
  }


}

/*
  +1>>  This source code is licensed as GPLv3 if not stated otherwise.
    >>  NO responsibility taken for ANY harm, damage done
    >>  to you, your data, animals, etc.
    >>
  +2>>
    >>  Last modified:  2014-02-20 :: 15:25
    >>  Origin: FugueGenerator
    >>
  +3>>
    >>  Copyright (c) 2014:
    >>
    >>     |             |     |
    >>     |    ,---.,---|,---.|---.
    >>     |    |   ||   |`---.|   |
    >>     `---'`---'`---'`---'`---'
    >>                    // Niklas Klügel
    >>
  +4>>
    >>  Made in Bavaria by fat little elves - since 1983.
 */

package FugueGenerator
import SessionLogLoader._

object FugueFilters {

  val ValenceFilter: SessionLogLoader.RowFilter = { row =>
    row(SessionLogLoader.RowEvtType) == "Event" && row(SessionLogLoader.RowMsg) == "Valence"
  }

  val ArousalFilter: SessionLogLoader.RowFilter = { row =>
    row(SessionLogLoader.RowEvtType) == "Event" && row(SessionLogLoader.RowMsg).contains("Arousal")
  }

}


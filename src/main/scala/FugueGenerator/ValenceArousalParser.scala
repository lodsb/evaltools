package FugueGenerator

import SessionLogLoader.{SessionLogLoader, RowParser, Exportable}

/*
  +1>>  This source code is licensed as GPLv3 if not stated otherwise.
    >>  NO responsibility taken for ANY harm, damage done
    >>  to you, your data, animals, etc.
    >>
  +2>>
    >>  Last modified:  2014-02-20 :: 15:34
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

class ValenceArousal(a1: Double, a2: Double, v: Double) extends Exportable{
  def toCSV: String = ""+v+dlmtr+a1+dlmtr+v+dlmtr+a2
  def csvDescriptor: String = "valence1"+dlmtr+"arousal1"+dlmtr+"valence2"+dlmtr+"arousal2"
}

class ValenceArousalParser extends RowParser[ValenceArousal] {
  var lastA1 = 0.0
  var lastA2 = 0.0
  var lastV = 0.0

  def process(row: SessionLogLoader.Row): ValenceArousal = {
    val msg = row(SessionLogLoader.RowMsg)
    row.foreach(x => print(x))
    println("--")


    // lazy ...
    if(msg.contains("Valence")) {
      lastV = row(SessionLogLoader.RowPayload).toDouble
    } else {
      if(msg.contains("1")) {
        lastA1 = row(SessionLogLoader.RowPayload).toDouble
      } else {
        lastA2 = row(SessionLogLoader.RowPayload).toDouble
      }
    }

    new ValenceArousal(lastA1, lastA2, lastV)
  }
}

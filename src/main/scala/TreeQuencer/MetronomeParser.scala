package TreeQuencer

import SessionLogLoader._

class Metronome(val duration: Double, val name: String ) extends Exportable {
  override def csvDescriptor = "dur"+dlmtr+"name"
  override def toCSV = {
    ""+duration+dlmtr+name
  }
}

object MetronomeParser extends RowParser[Metronome] {
  def process(row: SessionLogLoader.Row): Metronome = {
    new Metronome(row(SessionLogLoader.RowPayload).toDouble, row(SessionLogLoader.RowObj))
  }
}

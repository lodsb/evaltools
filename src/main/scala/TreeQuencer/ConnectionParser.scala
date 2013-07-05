package TreeQuencer

import SessionLogLoader._

class Connection(val src: Node, val dst: Node) extends Exportable {
  override def csvDescriptor = src.csvDescriptor+dlmtr+dst.csvDescriptor
  override def toCSV = {
    src.toCSV+dlmtr+dst.toCSV
  }
}

object ConnectionParser extends RowParser[Connection] {
  def process(r: SessionLogLoader.Row): Connection = {
    val dst = NodeParser.process(r(SessionLogLoader.RowObj))
    val src = NodeParser.process(r(SessionLogLoader.RowPayload))

    new Connection(src, dst)
  }
}

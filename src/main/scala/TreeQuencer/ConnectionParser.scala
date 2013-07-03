package TreeQuencer

import SessionLogLoader._

class Connection(src: Node, dst: Node)

object ConnectionParser extends RowParser[Connection] {
  def process(r: SessionLogLoader.Row): Connection = {
    val dst = NodeParser.process(r(SessionLogLoader.RowMsg))
    val src = NodeParser.process(r(SessionLogLoader.RowPayload))

    new Connection(src, dst)
  }
}

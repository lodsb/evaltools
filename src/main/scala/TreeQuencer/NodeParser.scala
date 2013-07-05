package TreeQuencer

import SessionLogLoader._

class Node(val id: String, val name: String) extends Exportable {
  override def csvDescriptor = "id"+dlmtr+"name"
  override def toCSV = {
    id+dlmtr+name
  }
}

object Node {
  var map = Map[String,  Node]()

  def apply(id: String, name: String) : Node = {
    if(!map.contains(id)) {
      val node = new Node(id, name)
      map += id -> node

      node
    } else {
      map.get(id).get
    }
  }

  def apply(id: String) = map.get(id)

}

object NodeParser extends StringParser[Node] {
  def process(s: String): Node = {
    val substr = s.split(".obj")
    val name = substr(0).trim
    val id = substr(1).split("@")(1).replace("]"," ").trim

    Node(id, name)
  }
}

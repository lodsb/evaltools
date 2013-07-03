package TreeQuencer

import SessionLogLoader._

class Node(val id: String, val name: String)

object NodeParser extends StringParser[Node] {
  def process(s: String): Node = {
    val substr = s.split(".obj")
    val name = substr(0).trim
    val id = substr(1).split("@")(1).replace("]"," ").trim

    new Node(id, name)
  }
}

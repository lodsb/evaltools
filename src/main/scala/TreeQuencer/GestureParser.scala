package TreeQuencer

import SessionLogLoader._

class Topology(val coord: (Float, Float, Float), val rot: (Float, Float, Float), val scale: Float) extends Exportable {
  override def csvDescriptor = "coordX"+dlmtr+"coordY"+dlmtr+"coordZ"+dlmtr+"rotX"+dlmtr+"rotY"+dlmtr+"rotZ"+dlmtr+"scale"

  override def toCSV = {
    ""+coord._1+dlmtr+coord._2+dlmtr+coord._3+dlmtr+rot._1+dlmtr+rot._2+dlmtr+rot._3+dlmtr+scale
  }
}
class Gesture(val t: String, val node: Node, val lenMilli: Long,
              val beginTopo: Topology,
              val endTopo: Topology,
              val posDev: Double, rotDev: Double, val scaleDev: Double ) extends Exportable {

  override def csvDescriptor = node.csvDescriptor+dlmtr+beginTopo.csvDescriptor+dlmtr+endTopo.csvDescriptor+dlmtr+"posDev"+dlmtr+"rotDev"+dlmtr+"scaleDev"+dlmtr+"lenMilli"

  override def toCSV = {
    node.toCSV+dlmtr+beginTopo.toCSV+dlmtr+endTopo.toCSV+dlmtr+posDev+dlmtr+rotDev+dlmtr+scaleDev+dlmtr+lenMilli
  }
}

object GestureParser extends Parser[(SessionLogLoader.Row, SessionLogLoader.Row), Gesture] {
  def process(rows: (SessionLogLoader.Row, SessionLogLoader.Row)): Gesture = {
    val beginRow = rows._1
    val endRow = rows._2

    val node = NodeParser.process(beginRow(SessionLogLoader.RowObj))
    val beforeTopo = TopologyParser.process(beginRow(SessionLogLoader.RowPayload))
    val endTopo = TopologyParser.process(endRow(SessionLogLoader.RowPayload))
    val t = GestureTypeParser.process(beginRow(SessionLogLoader.RowMsg))
    val tmilli = TimeParser.process(endRow(SessionLogLoader.RowMillisPassed))

    val deviations = TopologyParser.deviation(beforeTopo, endTopo)





    new Gesture(t, node, tmilli, beforeTopo, endTopo, deviations._1, deviations._2, deviations._3)
  }

}

object TopologyParser extends StringParser[Topology] {
  def process(s: String): Topology = {

    val top =  s.replace("W:","").replace("X:","").replace("Y:","").replace("Z:","")
                    .replace("(","").replace(")","").split(",")
                    .flatMap(x => x.trim.split(" ")).map(x => x.toFloat)


    return new Topology(coord = (top(0), top(1), top(2)), rot = (top(4), top(5), top(6)), scale = top(7))
  }


  def deviation(t1: Topology, t2: Topology) : (Double, Double, Double) = {
    def sq(f1: Float) = f1*f1;

    val posDev = scala.math.sqrt( sq(t2.coord._1 - t1.coord._1)
                                + sq(t2.coord._2 - t1.coord._2)
                                + sq(t2.coord._3 - t1.coord._3) )

    val rotDev = scala.math.sqrt( sq(t2.rot._1 - t1.rot._1)
                                + sq(t2.rot._2 - t1.rot._2)
                                + sq(t2.rot._3 - t1.rot._3) )

    val scaleDev = scala.math.sqrt( sq(t2.scale - t1.scale) )

    (posDev, rotDev, scaleDev)

  }

}

object GestureTypeParser extends StringParser[String] {
  def process(s: String): String = s.split("""\.""").filter( x => x.contains("@"))(0).split("@")(0)
}

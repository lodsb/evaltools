package TreeQuencer

import SessionLogLoader._
import SessionLogLoader.LogTable
import Filters._
import collection.mutable.ArrayBuffer
import scalax.collection.GraphEdge.DiEdge

class PathStatistics(val meanLength: Double, val stdVar: Double,
                     val medianLength: Double, val lengthHisto: Array[Int]) extends Exportable {

  override def csvDescriptor = {
    "meanLength" + dlmtr +
      "stdVar" + dlmtr +
      "medianLength" + SessionLogLoader.array2CSVHeader(lengthHisto, "h")
  }

  override def toCSV = {
    val histo = SessionLogLoader.array2CSV(lengthHisto)
    "" + meanLength + dlmtr + stdVar + dlmtr + medianLength + histo
  }
}

class GraphStatistics(val noNodes: Int, val noEdges: Int,
                      val avgDeg: Double, val varDeg: Double, val medianDeg: Double) extends Exportable {

  override def csvDescriptor = {
    "noNodes" + dlmtr + "noEdges" + dlmtr + "avgDeg" + dlmtr + "varDeg" + dlmtr + "medianDeg"
  }

  override def toCSV = {
    "" + noNodes + dlmtr + noEdges + dlmtr + avgDeg + dlmtr + varDeg + dlmtr + medianDeg
  }
}


class RhythmStatistics(val length: Int, val numEvents: Seq[Double], val oddity : Seq[Double], val wnbd: Seq[Double],
                       val meanOdd: Double, val meanWnbd: Double, val devOdd: Double,
                       val devWnbd: Double, val sumOdd: Double, val sumWnbd: Double ) extends Exportable {

  override def csvDescriptor = {
          "length" +
          SessionLogLoader.array2CSVHeader(numEvents, "n")  +
          SessionLogLoader.array2CSVHeader(oddity, "o") +
          SessionLogLoader.array2CSVHeader(wnbd, "w") + dlmtr +
          "meanOdd" + dlmtr +
          "meanWnbd" + dlmtr +
          "devOdd" + dlmtr +
          "devWnbd" + dlmtr +
          "sumOdd" + dlmtr +
          "sumWnbd"
  }

  override def toCSV = {
          ""+length +
          SessionLogLoader.array2CSV(numEvents) +
          SessionLogLoader.array2CSV(oddity) +
          SessionLogLoader.array2CSV(wnbd) + dlmtr +
          meanOdd + dlmtr +
          meanWnbd + dlmtr +
          devOdd + dlmtr +
          devWnbd + dlmtr +
          sumOdd + dlmtr +
          sumWnbd
    }
}

object TreeQuencerLogAnalyzer extends App {

  var synthNodeTypes: Seq[String] =  null

  def creationSequence(log: LogTable): Seq[(Long, Node)] = {
    val sequence = log.filter(x => NodeCreationFilter(x)).map(

      x => (TimeParser.process(x(SessionLogLoader.RowMillis)))
        ->
        NodeParser.process(x(SessionLogLoader.RowObj))
    )

    sequence.sortWith((x, y) => (x._1 < y._1))
  }

  def rootNodes(creationSeq: Seq[(Long, Node)]): Seq[Node] = {
    creationSeq.filter(x => x._2.name == "center").map(x => x._2)
  }

  def removalSequence(log: LogTable): Seq[(Long, Node)] = {
    val sequence = log.filter(x => NodeRemovalFilter(x)).map(

      x => (TimeParser.process(x(SessionLogLoader.RowMillis)))
        ->
        NodeParser.process(x(SessionLogLoader.RowObj))
    )

    sequence.sortWith((x, y) => (x._1 < y._1))
  }


  def additionToSetSequence(log: LogTable): Seq[(Long, Connection)] = {
    val sequence = log.filter(x => NodeSetAddFilter(x)).map(

      x => (TimeParser.process(x(SessionLogLoader.RowMillis)))
        ->
        ConnectionParser.process(x)
    )

    sequence.sortWith((x, y) => (x._1 < y._1))
  }


  def removalFromSetSequence(log: LogTable): Seq[(Long, Connection)] = {
    val sequence = log.filter(x => NodeSetRemovalFilter(x)).map(

      x => (TimeParser.process(x(SessionLogLoader.RowMillis)))
        ->
        ConnectionParser.process(x)
    )

    sequence.sortWith((x, y) => (x._1 < y._1))
  }

  def gestureSequence(log: LogTable): Seq[(Long, Gesture)] = {
    val beginGestures = log.filter(x => BeginGestureFilter(x))
    val endGestures = log.filter(x => EndGestureFilter(x))


    var gestureList = ArrayBuffer[(SessionLogLoader.Row, SessionLogLoader.Row)]()

    beginGestures.foreach({
      row =>
        val gestureType = row(SessionLogLoader.RowObj)
        val gestureStart = TimeParser.process(row(SessionLogLoader.RowMillis))

//        println("gesture type " + gestureType + " s "+gestureStart)

        // search same gestures taking place later than begin
        var sameEndGesture: Seq[SessionLogLoader.Row] = endGestures.filter(
          erow =>
            (erow(SessionLogLoader.RowObj) == gestureType)
              && (gestureStart < TimeParser.process(erow(SessionLogLoader.RowMillis)))
        )

//        println("------")
//        endGestures.foreach({
//                  erow =>
//                    println("---")
//                    println(erow(SessionLogLoader.RowObj) + " | " + gestureType + " FT " + (erow(SessionLogLoader.RowObj) == gestureType)+" lll " )
//                    println("###")
//                    println(TimeParser.process(erow(SessionLogLoader.RowMillis))+ " | " + gestureStart + " FT "+ (gestureStart < TimeParser.process(erow(SessionLogLoader.RowMillis))))
//                    println("+++")
//        })


//        println("SAME ENDE GESTURE")
//        println(sameEndGesture)
        sameEndGesture = sameEndGesture.sortWith((x, y) => x(SessionLogLoader.RowMillis) < y(SessionLogLoader.RowMillis))

        if (sameEndGesture.size >= 1) {
          gestureList += (row -> sameEndGesture(0))
        }
    }

    )


    val gestureSequence = gestureList.map({
      x =>
        val beginRow = x._1
        val endRow = x._2

        (TimeParser.process(beginRow(0)), GestureParser.process(beginRow, endRow))
    })

    gestureSequence.sortWith((x, y) => (x._1 < y._1))
  }

  def metroChangedSequence(log: LogTable): Seq[(Long, Metronome)] = {
    val sequence = log.filter(x => MetronomeChangedFilter(x)).map(

      x => (TimeParser.process(x(SessionLogLoader.RowMillis)))
        ->
        MetronomeParser.process(x)
    )

    sequence.sortWith((x, y) => (x._1 < y._1))
  }

  object GraphOperationType extends Enumeration {
    type GraphOperationType = Value
    val AddN, RmN, AddC, RmC = Value
  }

  import GraphOperationType._
  import scalax.collection.immutable.Graph
  import scalax.collection.GraphPredef._

  case class GraphOperation(p: GraphOperationType, src: Node, dst: Node)

  def generateTimeStepGraphs(crNodes: Seq[(Long, Node)],
                             rmNodes: Seq[(Long, Node)],
                             connectNodes: Seq[(Long, Connection)],
                             disconnectNodes: Seq[(Long, Connection)]): Seq[(Long, Graph[Node, DiEdge])] = {

    val crNOps = crNodes.map(x => (x._1, GraphOperation(AddN, x._2, null)))
    val rmNOps = rmNodes.map(x => (x._1, GraphOperation(RmN, x._2, null)))
    val addCOps = connectNodes.map(x => (x._1, GraphOperation(AddC, x._2.src, x._2.dst)))
    val rmCOps = disconnectNodes.map(x => (x._1, GraphOperation(RmC, x._2.src, x._2.dst)))


    val allOps: Seq[(Long, GraphOperation)] = (crNOps ++ rmNOps ++ addCOps ++ rmCOps).sortWith((x, y) => x._1 < y._1)

//    println("ALL OPS")
//    allOps.foreach(println(_))
//    println("#####")

    // immutable graph, so we can build it up in a step-wise manner
    var currentGraph = Graph[Node, DiEdge]()
    val graphs = allOps.map({
      gop =>
        val op = gop._2
        val time = gop._1

        op match {
          case GraphOperation(AddN, srcN, _) => currentGraph = currentGraph + srcN
          case GraphOperation(RmN, srcN, _) => currentGraph = currentGraph - srcN
          case GraphOperation(AddC, srcN, dstN) => currentGraph = currentGraph + dstN ~> srcN
          case GraphOperation(RmC, srcN, dstN) => currentGraph = currentGraph - (dstN ~> srcN)
          case _ => println("foo")
        }

        (time, currentGraph)


    })




    graphs

  }

  def mean(seq: Seq[Double]): Double = seq.reduce((_ + _)) / seq.size.toDouble

  def stdDev(seq: Seq[Double]): Double = {
    val avg = mean(seq)
    scala.math.sqrt((seq.map(x => (x - avg) * (x - avg)).sum) / (seq.length - 1).toDouble)
  }

  def median(seq: Seq[Double]): Double = seq(seq.length / 2)

  // The lazy way...
  def pathStatistics(g: Graph[Node, DiEdge]): PathStatistics = {
    val nodes = g.nodes
    val nodes2 = g.nodes
    val pathsHistogram = new Array[Int](20);
    var pathLengthList = List[Double]();

    println(nodes.size)

    nodes.foreach({
      n1 =>
        nodes2.foreach({
          n2 =>
            val p = n1 pathTo n2
//            println(p)
            if (!p.isEmpty) {
              val plength = p.get.edges.length
//              println("plength "+plength)
              val idx = plength - 1

              pathsHistogram(idx) = pathsHistogram(idx) + 1;

              pathLengthList = pathLengthList :+ plength.toDouble

//              println(pathLengthList)
            }
        })
    })

    var avg = 0.0;
    var med = 0.0;
    var variance = 0.0;

    if (!pathLengthList.isEmpty) {
      avg = mean(pathLengthList)
      variance = stdDev(pathLengthList)
      med = median(pathLengthList)
    }

    new PathStatistics(avg, variance, med, pathsHistogram)
  }

  def graphStatistics(g: Graph[Node, DiEdge]): GraphStatistics = {
    val noNodes = g.order
    val noEdges = g.size
    val degrees = g.degreeNodeSeq(g.OutDegree).map(x => x._1.toDouble)

    val avgDegrees = mean(degrees)
    val varDegrees = stdDev(degrees)
    val medianDegrees = median(degrees)

    new GraphStatistics(noNodes, noEdges, avgDegrees, varDegrees, medianDegrees)
  }

  def rhythmStatistics(sc: RhythmReconstruction.Score): RhythmStatistics = {
    val length = sc.length

    val numEvents = RhythmComplexity.numEvents(sc)
    val oddity = RhythmComplexity.rhythmicOddity(sc)
    val wnbd = RhythmComplexity.wnbd(sc, 4)

    val meanOdd = mean(oddity)
    val meanWndb= mean(wnbd)

    val devOdd = stdDev(oddity)
    val devWndb= stdDev(wnbd)

    val sumOdd = oddity.sum
    val sumWnbd= wnbd.sum

    new RhythmStatistics(length, numEvents, oddity, wnbd, meanOdd, meanWndb,
                         devOdd, devWndb, sumOdd, sumWnbd)


  }

  def stateOfNodeAtTimestamp(ges: Seq[(Long, Gesture)], node: Node, ts: Long): Option[Topology] = {
    // the lazy way

    //println("GESTURES")
    //println(ges)

    var state = None

    val search = ges.filter({
      x =>
        x._1 <= ts &&x._2.node.id == node.id
    })

    //println("SEARCH "+search + " NODE "+node.id + " | "+node.name)

    if (search.length == 0)
      None
    else
      Some(search(search.length - 1)._2.endTopo) // last update on that Node
  }

  def pulseInverseFrequencyOfNode(ges: Seq[(Long, Gesture)], ts: Long)(node: Node): Int = {
    val topOp = stateOfNodeAtTimestamp(ges, node, ts)

    if (!topOp.isEmpty) {
      val topology = topOp.get
      distanceToSteps(topology.coord)
    } else {
      1
    }
  }

  private def distanceToSteps(nodePosition: (Float, Float, Float)): Int = {
    val appHeight = 1080f
    val appWidth = 1920f

    val granulation = 8
    val centerPosition = (appWidth / 2f, appHeight / 2f)

    val xdist = nodePosition._1 - centerPosition._1
    val ydist = nodePosition._2 - centerPosition._2
    val distance = scala.math.sqrt((xdist * xdist) + (ydist * ydist))

    val maxDistance = List(appHeight, appWidth).min / 2f

    var triggerTime = math.round(distance / maxDistance * (granulation - 1) + 1)
    if (triggerTime > granulation) {
      triggerTime = granulation
    }


    triggerTime.toInt
  }


  def analyze(filename: String, game: Int, path: String) = {
    val log = SessionLogLoader.load(filename)

    val crs = creationSequence(log)
    val rms = removalSequence(log)
    val ass = additionToSetSequence(log)
    val rmss = removalFromSetSequence(log)
    val ges = gestureSequence(log)
    val mes = metroChangedSequence(log)

    val grs = generateTimeStepGraphs(crs, rms, ass, rmss)

    val grstat = grs.map(x => (x._1, graphStatistics(x._2)))
    val pstat = grs.map(x => (x._1, pathStatistics(x._2)))

    val rootN = rootNodes(crs)

    rootN.foreach(println(_))

    val rhythmstat = grs.map(x => {

      val score = game match {
        case 0 => RhythmReconstruction.game0(rootN, x._2)
        case 1 => RhythmReconstruction.game1(rootN, x._2)
        case 2 => RhythmReconstruction.game2(rootN, x._2, pulseInverseFrequencyOfNode(ges, x._1))
      }
      //RhythmReconstruction.printScore(score)
      RhythmReconstruction.dumpScore(path+"score_at_"+x._1+".csv", score)

      ( x._1, rhythmStatistics(score) )
    })

    val fileMap = Map(
        "creation" -> crs,
        "removal" -> rms,
        "additionToSet" -> ass,
        "removalFromSet" -> rmss,
        "gestures" -> ges ,
        "metronome" -> mes,
        "graphStat" -> grstat,
        "pathStat" -> pstat,
        "rhythmStat" -> rhythmstat
    )


    SessionLogLoader.writeSeqCollection(path, fileMap)


  }


  override def main(args: Array[String]) = {
    import java.io.File

    if (args.length != 3) {
      println("Missing arguments, should be: <log> <gametype (Number)> <outputPath>")
    } else {
      val log = args(0)
      val game= args(1).toInt
      val path = args(2)

      if (game > 2 || game < 0) {
        println("Wrong game number")
      } else if(! (new File(log)).exists ) {
        println("Log does not exist")
      } else {

        synthNodeTypes =  Array("angular_something", "cube", "pentagon", "iconosphere", "tetrahedron", "decimate")

        analyze(log, game, path)
      }
    }

  }




}

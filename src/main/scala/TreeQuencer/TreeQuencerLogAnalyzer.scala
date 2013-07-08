package TreeQuencer
import SessionLogLoader._
import SessionLogLoader.LogTable
import Filters._
import collection.mutable.ArrayBuffer
import scalax.collection.GraphEdge.DiEdge

class PathStatistics(val meanLength: Double, val stdVar: Double,
                     val medianLength:Double, val lengthHisto: Array[Int]) extends Exportable {

  override def csvDescriptor = {"meanLength"+dlmtr+
                                "stdVar"+dlmtr+
                                "medianLength"+((1 to lengthHisto.size).foreach( x => dlmtr+"h"+x))
  }

  override def toCSV = {
    var histo = lengthHisto.foldLeft("")( (x,y) => x+dlmtr+y )
    ""+meanLength+dlmtr+stdVar+dlmtr+medianLength+lengthHisto
  }
}

class GraphStatistics(val noNodes: Int, val noEdges: Int,
                      val avgDeg: Double, val varDeg: Double, val medianDeg: Double ) extends Exportable {

  override def csvDescriptor = {
    "noNodes"+dlmtr+"noEdges"+dlmtr+"avgDeg"+dlmtr+"varDeg"+dlmtr+"medianDeg"
  }

  override def toCSV = {
    ""+noNodes+dlmtr+noEdges+dlmtr+avgDeg+dlmtr+varDeg+dlmtr+medianDeg
  }
}

object TreeQuencerLogAnalyzer extends App {

  var synthNodeTypes: Seq[String] = Array("angular_something", "cube", "pentagon", "iconosphere", "tetrahedron", "decimate")


  def creationSequence(log: LogTable) : Seq[(Long,  Node)]=  {
    val sequence =  log.filter(x => NodeCreationFilter(x)).map(

      x=> ( TimeParser.process(x(SessionLogLoader.RowMillis)))
            ->
            NodeParser.process(x(SessionLogLoader.RowObj))
    )

    sequence.sortWith( (x,y) => (x._1 < y._1) )
  }

  def rootNodes(creationSeq : Seq[(Long,Node)]): Seq[Node] = {
    creationSeq.filter(x => x._2.name == "center").map( x => x._2)
  }

  def removalSequence(log: LogTable) : Seq[(Long,  Node)]=  {
    val sequence =  log.filter(x => NodeRemovalFilter(x)).map(

      x=> ( TimeParser.process(x(SessionLogLoader.RowMillis)))
            ->
            NodeParser.process(x(SessionLogLoader.RowObj))
    )

    sequence.sortWith( (x,y) => (x._1 < y._1) )
  }


  def additionToSetSequence(log: LogTable) : Seq[(Long,  Connection)]=  {
    val sequence =  log.filter(x => NodeSetAddFilter(x)).map(

      x=> ( TimeParser.process(x(SessionLogLoader.RowMillis)))
            ->
            ConnectionParser.process(x)
    )

    sequence.sortWith( (x,y) => (x._1 < y._1) )
  }


  def removalFromSetSequence(log: LogTable) : Seq[(Long,  Connection)]=  {
    val sequence =  log.filter(x => NodeSetRemovalFilter(x)).map(

      x=> ( TimeParser.process(x(SessionLogLoader.RowMillis)))
            ->
            ConnectionParser.process(x)
    )

    sequence.sortWith( (x,y) => (x._1 < y._1) )
  }

  def gestureSequence(log: LogTable) : Seq[(Long,  Gesture)] = {
    val beginGestures = log.filter(x => BeginGestureFilter(x))
    val endGestures = log.filter(x => EndGestureFilter(x))

    var gestureList = ArrayBuffer[(SessionLogLoader.Row, SessionLogLoader.Row)]()

    beginGestures.foreach( { row =>
      val gestureType = row(SessionLogLoader.RowObj)
      val gestureStart= TimeParser.process(row(SessionLogLoader.RowMillis))

      // search same gestures taking place later than begin
      var sameEndGesture: Seq[SessionLogLoader.Row] = endGestures.filter(
        erow =>
        (erow(SessionLogLoader.RowObj) == gestureType)
         && (gestureStart < TimeParser.process(erow(SessionLogLoader.RowMillisPassed)))
      )

      sameEndGesture = sameEndGesture.sortWith( (x,y) => x(SessionLogLoader.RowMillis) < y(SessionLogLoader.RowMillis))

      if (sameEndGesture.size >= 1) {
        gestureList += ( row -> sameEndGesture(0) )
      }
    }

    )


    val gestureSequence = gestureList.map({ x =>
      val beginRow = x._1
      val endRow = x._2

      (TimeParser.process(beginRow(0)), GestureParser.process(beginRow, endRow))
    })

    gestureSequence.sortWith( (x,y) => (x._1 < y._1) )
  }

  def metroChangedSequence(log: LogTable) : Seq[(Long,  Metronome)]=  {
    val sequence =  log.filter(x => MetronomeChangedFilter(x)).map(

      x=> ( TimeParser.process(x(SessionLogLoader.RowMillis)))
            ->
            MetronomeParser.process(x)
    )

    sequence.sortWith( (x,y) => (x._1 < y._1) )
  }

  object GraphOperationType extends Enumeration {
    type GraphOperationType = Value
    val AddN, RmN, AddC, RmC = Value
  }

  import GraphOperationType._
  import scalax.collection.immutable.Graph
  import scalax.collection.GraphPredef._

  case class GraphOperation(p: GraphOperationType, src: Node, dst: Node)

  def generateTimeStepGraphs(crNodes: Seq[(Long,  Node)],
                             rmNodes: Seq[(Long,  Node)],
                             connectNodes: Seq[(Long,  Connection)],
                             disconnectNodes: Seq[(Long,  Connection)]) : Seq[(Long,Graph[Node, DiEdge])]= {

    val crNOps = crNodes.map( x => (x._1, GraphOperation(AddN, x._2, null)))
    val rmNOps = rmNodes.map( x => (x._1, GraphOperation(RmN, x._2, null)))
    val addCOps = connectNodes.map( x => (x._1, GraphOperation(AddC, x._2.src, x._2.dst)))
    val rmCOps = connectNodes.map( x => (x._1, GraphOperation(RmC, x._2.src, x._2.dst)))


    val allOps: Seq[(Long, GraphOperation)] = (crNOps ++ rmNOps++ addCOps ++ rmCOps).sortWith( (x,y) => x._1 < y._1)

    // immutable graph, so we can build it up in a step-wise manner
    var currentGraph = Graph[Node, DiEdge]()
    val graphs = allOps.map({ gop =>
      val op = gop._2
      val time=gop._1

      op match {
        case GraphOperation(AddN, srcN, _) =>   currentGraph = currentGraph + srcN
        case GraphOperation(RmN, srcN, _) =>    currentGraph = currentGraph - srcN
        case GraphOperation(AddC, srcN, dstN) => currentGraph = currentGraph + dstN ~> srcN
        case GraphOperation(RmC, srcN, dstN) => currentGraph = currentGraph -(srcN ~> dstN)
      }

      (time , currentGraph)


    })




    graphs

  }

  def mean(seq: Seq[Double])  : Double = seq.reduce( ( _ + _ ) ) / seq.size.toDouble

  def stdDev(seq: Seq[Double]): Double = {
    val avg = mean(seq)
    scala.math.sqrt( ( seq.map( x => (x - avg)*(x - avg) ).sum ) / (seq.length-1).toDouble )
  }

  def median(seq: Seq[Double]) : Double = seq(seq.length/2)

  // The lazy way...
  def pathStatistics(g: Graph[Node, DiEdge]) : PathStatistics = {
    val nodes = g.nodes
    val nodes2= g.nodes
    val pathsHistogram = Array[Int](20);
    var pathLengthList = List[Double]();

    println(nodes.size)

    nodes.foreach({   n1 =>
      nodes2.foreach({ n2 =>
        val p = n1 pathTo n2
        if(!p.isEmpty) {
          val plength = p.toList.length
          val idx = plength -1

          pathsHistogram(idx) = pathsHistogram(idx) + 1;

          pathLengthList = pathLengthList :+ plength.toDouble
        }
      })
    })

    var avg = 0.0;
    var med = 0.0;
    var variance= 0.0;

    if(!pathLengthList.isEmpty) {
      avg       = mean(pathLengthList)
      variance  = stdDev(pathLengthList)
      med    = median(pathLengthList)
    }

    new PathStatistics(avg, variance, med, pathsHistogram)
  }

  def graphStatistics(g: Graph[Node, DiEdge]) : GraphStatistics = {
    val noNodes = g.order
    val noEdges = g.size
    val degrees = g.degreeNodeSeq(g.OutDegree).map( x => x._1.toDouble)

    val avgDegrees = mean(degrees)
    val varDegrees = stdDev(degrees)
    val medianDegrees = median(degrees)

    new GraphStatistics(noNodes, noEdges, avgDegrees, varDegrees, medianDegrees)
  }


  def analyze(filename: String) = {
    val log = SessionLogLoader.load(filename)

    val crs = creationSequence(log)
    val rms = removalSequence(log)
    val ass = additionToSetSequence(log)
    val rmss= removalFromSetSequence(log)
    val ges  = gestureSequence(log)
    val mes  = metroChangedSequence(log)

    val grs  = generateTimeStepGraphs(crs, rms, ass, rmss)

    val grstat= grs.map(x => (x._1, graphStatistics(x._2) ) )
    val pstat= grs.map(x => (x._1, pathStatistics(x._2) ) )

    val fileMap =  Map( "creation"      -> crs,
                        "removal"       ->rms,
                        "additionToSet" -> ass,
                        "removalFromSet"-> rmss,
                        "gestures"      -> ges,
                        "metronome"     -> mes,
                        "graphStat"     -> grstat,
                        "pathStat"      -> pstat
                    )


    SessionLogLoader.printSeqCollection(fileMap)

    val rootN = rootNodes(crs)

    rootN.foreach(println(_))

    grs.map( x => RhythmReconstruction.printScore(RhythmReconstruction.game0And2(rootN, x._2, {x:Node => 0} ) ) )
  }

}


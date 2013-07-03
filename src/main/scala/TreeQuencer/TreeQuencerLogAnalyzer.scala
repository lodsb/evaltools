package TreeQuencer
import SessionLogLoader._
import SessionLogLoader.LogTable
import Filters._
import collection.mutable.ArrayBuffer

object TreeQuencerLogAnalyzer {

  def allCreatedNodesMap(log: LogTable) : Map[String,  Node]=  {
    val nodesCreated =  log.filter(x => NodeCreationFilter(x)).map(
      x=> NodeParser.process(x(SessionLogLoader.RowObj)))

    var map = Map[String,  Node]()

    nodesCreated.foreach( node => map +=  node.id ->  node)

    map
  }

  def creationSequence(log: LogTable) : Seq[(Long,  Node)]=  {
    val sequence =  log.filter(x => NodeCreationFilter(x)).map(

      x=> ( TimeParser.process(x(SessionLogLoader.RowMillis)))
            ->
            NodeParser.process(x(SessionLogLoader.RowObj))
    )

    sequence.sortWith( (x,y) => (x._1 < y._1) )
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

  def gestureSequence(log: LogTable) = {
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

}

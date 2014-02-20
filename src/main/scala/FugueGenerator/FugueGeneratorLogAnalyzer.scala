package FugueGenerator

import SessionLogLoader._
import SessionLogLoader.LogTable
import TreeQuencer.{TimeParser, Filters}
import collection.mutable.ArrayBuffer
import scalax.collection.GraphEdge.DiEdge



object FugueGeneratorLogAnalyzer extends App {

  def vaSequence(log: LogTable) : Seq[(Long, ValenceArousal)] = {
    // filter for va events and attach time, so we can order it by time
    val vseq = log.filter( x=> FugueFilters.ValenceFilter(x)).map{
      x => TimeParser.process(x(SessionLogLoader.RowMillis)) -> x
    }

    val aseq = log.filter( x=> FugueFilters.ArousalFilter(x)).map{
      x => TimeParser.process(x(SessionLogLoader.RowMillis)) -> x
    }


    val parser = new ValenceArousalParser

    // concatenate, order by time and parse rows
    val vaseq = (vseq ++ aseq).sortWith((x, y) => (x._1 < y._1)).map {
      x => x._1 -> parser.process(x._2)
    }


    vaseq
  }

   def analyze(filename: String, path: String) = {
    val log = SessionLogLoader.load(filename)

    val vas = vaSequence(log)
    //val ms = msSequence(log)

    val fileMap = Map(
        "ValenceArousal" -> vas
        //"Melody" -> ms
    )


    SessionLogLoader.writeSeqCollection(path, fileMap)


  }


  override def main(args: Array[String]) = {
    import java.io.File

    if (args.length != 2) {
      println("Missing arguments, should be: <log> <outputPath>")
    } else {
      val log = args(0)
      val path = args(1)

      if(! (new java.io.File(log)).exists ) {
        println("Log does not exist")
      } else {
        analyze(log, path)
      }
    }

  }




}

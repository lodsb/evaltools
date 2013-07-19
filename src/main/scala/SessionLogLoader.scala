package SessionLogLoader

import io.Source
import java.io.PrintWriter

trait Exportable {
  val dlmtr = SessionLogLoader.csvDelimiter
  def toCSV : String
  def csvDescriptor: String
}

trait Parser[T,V] {
  def process(b: T) : V
}
trait RowParser[T] extends Parser[SessionLogLoader.Row, T] {
  def process(row : SessionLogLoader.Row): T
}

trait StringParser[T] {
  def process(s : String): T
}

object SessionLogLoader {
  val csvDelimiter = " , "

  type Row = Array[String]
  type LogTable = Array[Row]

  type RowFilter = SessionLogLoader.Row => Boolean

  type CSV = Seq[String]

  val RowMillis = 0;
  val RowEvtType = 1;
  val RowMsg= 2;
  val RowObj= 3;
  val RowMillisPassed= 4;
  val RowPayload= 5;

  def array2CSV(arr: Seq[_ <: AnyVal]) : String = {
    arr.foldLeft("")((x, y) => x + SessionLogLoader.csvDelimiter + y)
  }

  def array2CSVHeader(arr: Seq[_ <: AnyVal], header: String) : String = {
    (1 to arr.size).foldLeft(""){(x,y) => x +SessionLogLoader.csvDelimiter+ header  + y  }
  }


  def writeToFile(path: String, contents: String) = {
    Some(new PrintWriter(path)).foreach{p => p.write(contents); p.close}
  }

  def sequenceToCSV(seq: Seq[(Long, Exportable)]): CSV = {
    if (!seq.isEmpty)
      Seq("time"+SessionLogLoader.csvDelimiter+seq(0)._2.csvDescriptor) ++ seq.map( x => x._1+SessionLogLoader.csvDelimiter+x._2.toCSV)
    else
      Seq()
  }

  def printCSV(csv: CSV) = {
    csv.foreach(x => println(x))
  }

  def CSVtoString(csv: CSV) = {
    var string = ""
    csv.foreach(x => string = string + x+"\n")

    string
  }

  def printSeqCollection(coll: Map[String, Seq[(Long, Exportable)]]) = {
    coll.foreach({ x => println("---"+x._1+"---"); printCSV(sequenceToCSV(x._2)) })
  }

  def writeSeqCollection(path: String, coll: Map[String, Seq[(Long, Exportable)]]) = {
    coll.foreach({ x => writeToFile(path+x._1+".csv", CSVtoString(sequenceToCSV(x._2))) })
  }

  def load(filename: String)  = {
    val table: LogTable = Source.fromFile(filename).getLines().map(
      x => x.split("""\|""").map(s=>s.replace('\"', ' ').trim)
    ).toArray.drop(1) // drop csv header

    table
  }
}

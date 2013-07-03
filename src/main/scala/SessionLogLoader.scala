package SessionLogLoader

import io.Source

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
  type Row = Array[String]
  type LogTable = Array[Row]

  type RowFilter = SessionLogLoader.Row => Boolean


  val RowMillis = 0;
  val RowEvtType = 1;
  val RowMsg= 2;
  val RowObj= 3;
  val RowMillisPassed= 4;
  val RowPayload= 5;



  def load(filename: String)  = {
    val table: LogTable = Source.fromFile(filename).getLines().map(
      x => x.split("""\|""").map(s=>s.replace('\"', ' ').trim)
    ).toArray.drop(1) // drop csv header

    table
  }
}

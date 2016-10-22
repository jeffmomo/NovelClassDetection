import Helpers._

/**
  * Created by jeffmo on 16/10/16.
  */
object RecordReader {

  case class Record(data: RecordData, label: Label)

  def getRecords(filename: String): Array[Record] = {

    io.Source.fromFile(filename).getLines().toArray.map((a) => {
      val splitted = a.split(',').map(_.toDouble)

      Record(splitted.slice(0, splitted.length - 1), splitted.last)
    })
  }

}

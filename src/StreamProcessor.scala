import Helpers._

/**
  * Created by jeffmo on 16/10/16.
  */

trait StreamTransformer[OptionsType] {
  def transform[OptionsType](a: Array[RecordData], options: OptionsType): Array[RecordData]
}

//object Binarizer extends StreamTransformer[scala.collection.mutable.Map[Int, Int]] {
//
//  override def transform(record:Array[RecordData], attributeMap: scala.collection.mutable.Map[Int, Int]): Array[RecordData] = {
//
//    var idx = -1
//    record.flatMap((a) => {
//      idx += 1
//
//      attributeMap.get(idx) match {
//        case Some(value) => for (i <- 0 until value) yield {
//          if (i == a) 1.0 else 0.0
//        }
//        case _ => Array(a)
//      }
//    })
//  }
//}
//
//
//
//
//

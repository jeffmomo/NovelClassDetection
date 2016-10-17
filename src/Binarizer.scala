/**
  * Created by jeffmo on 16/10/16.
  */
object Binarizer {

  def binarize(record:Array[Double], attributeMap: scala.collection.mutable.Map[Int, Int]): Array[Double] = {

    var idx = -1
    record.flatMap((a) => {
      idx += 1

      attributeMap.get(idx) match {
        case Some(value) => for (i <- 0 until value) yield {
          if (i == a) 1.0 else 0.0
        }
        case _ => Array(a)
      }
    })

  }

}

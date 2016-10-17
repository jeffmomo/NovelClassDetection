import scala.collection.mutable.ArrayBuffer

/**
  * Created by jeffmo on 17/10/16.
  */
object Helpers {

  trait DistanceMetric {
    def distance(a: Record, b: Record): Double
  }


  object EuclideanDistance extends DistanceMetric {

    override def distance(a: Record, b: Record): Double = {

      val sumArray = (a zip b) map (tuple => Math.pow(tuple._1 - tuple._2, 2))

      Math.sqrt(sumArray.sum)
    }

  }


  type Record = Array[Double]

  implicit class AugmentedRecords(v: ArrayBuffer[Record]) {

    private def getQMeanDistance(q:Int, x:Record, records:Array[Record])(implicit metric:DistanceMetric): Double = {
      val distances = records.map(metric.distance(_, x)).sorted

      (for(i <- 0 until q) yield distances(i)).sum / q
    }

    def qSNC(q: Int, x:Record, outliers: Array[Record], existing: Array[Record])(implicit metric:DistanceMetric): Double = {

      assert(outliers.length >= q && existing.length >= q)

      val outlierDistance = getQMeanDistance(q, x, outliers)
      val existingClassDistance = getQMeanDistance(q, x, existing)

      (existingClassDistance - outlierDistance) / (existingClassDistance max outlierDistance)
    }

  }

}

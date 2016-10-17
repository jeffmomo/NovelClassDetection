import scala.collection.mutable.ArrayBuffer

/**
  * Created by jeffmo on 17/10/16.
  */
object Helpers {

  trait DistanceMetric {
    def distance(a: RecordData, b: RecordData): Double
    // a == b -> 0
    // d(a, b) == d(b, a)
  }


  object EuclideanDistance extends DistanceMetric {

    override def distance(a: RecordData, b: RecordData): Double = {

      val sumArray = (a zip b) map (tuple => Math.pow(tuple._1 - tuple._2, 2))

      Math.sqrt(sumArray.sum)
    }

  }

  def gini(cdf: IndexedSeq[Double]): Double = {

    var numeratorSum = 0.0
    var denominatorSum = 0.0
    for (i <- cdf.indices) {
      numeratorSum += (cdf.length - i) * cdf(i)
      denominatorSum += cdf(i)
    }


    (cdf.length + 1 - (2 * (numeratorSum / denominatorSum))) / cdf.length
  }

  def silhouetteCoefficient(nnDist: Double, meanDist:Double): Double = {
    val residual = nnDist - meanDist

    residual / (residual max meanDist)
  }


  def qSNC(q: Int, x:RecordData, outliers: Array[RecordData], existing: Array[RecordData])(implicit metric:DistanceMetric): Double = {

    assert(outliers.length >= q && existing.length >= q)

    val outlierDistance = getQMeanDistance(q, x, outliers)
    val existingClassDistance = getQMeanDistance(q, x, existing)

    (existingClassDistance - outlierDistance) / (existingClassDistance max outlierDistance)
  }

  private def getQMeanDistance(q:Int, x:RecordData, records:Array[RecordData])(implicit metric:DistanceMetric): Double = {
    val distances = records.map(metric.distance(_, x)).sorted

    (for(i <- 0 until q) yield distances(i)).sum / q
  }


  type RecordData = Array[Double]
  type Label = Double

  implicit class AugmentedRecords(v: IndexedSeq[RecordData]) {

  }

}

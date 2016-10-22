import RecordReader.Record

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

  // OUTTH class for maintaining the threshould throughout the entire application
  class OUTTH(initial: Double = 0.7, epsilon: Double, comparisonEpsilon: Double, clamp: Boolean = true) {

    private var threshold = initial

    val eps = epsilon
    val compEps = comparisonEpsilon

    // getter
    def value = threshold

    def adjustFalseNovel(weight: Double): Unit = {
      assert(weight < threshold)

      // increase slack space
      if(threshold - weight < comparisonEpsilon)
        threshold -= epsilon
    }

    def adjustFalseExisting(): Unit = {
      threshold += eps

      if(clamp && threshold > 1)
        threshold = 1
    }
  }


  object EuclideanDistance extends DistanceMetric {

    override def distance(a: RecordData, b: RecordData): Double = {
      assert(a.length == b.length, "records have uneven lengths!?")

      // java-like for performance
      var sum = 0.0
      val max = a.length
      var idx = 0
      while(idx < max) {
        val diff = a(idx) - b(idx)
        sum += diff * diff

        idx += 1
      }

      Math.sqrt(sum)
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

  def qSNC(q: Int, x:Record, outliers: Array[Record], existing: Array[Record])(implicit metric:DistanceMetric): Double = {

    assert(outliers.length == q && existing.length >= q, "not correct amount of records to calculate QSNC")

    val outlierDistance = getQMeanDistance(q, x, outliers)
    val existingClassDistance = getQMeanDistance(q, x, existing)

    (existingClassDistance - outlierDistance) / (existingClassDistance max outlierDistance)
  }

  private def getQMeanDistance(q:Int, x:Record, records:Array[Record])(implicit metric:DistanceMetric): Double = {
    val distances = records.map((a) => metric.distance(x.data, a.data)).sorted

    (for(i <- 0 until (q min records.length)) yield distances(i)).sum / q
  }


  // typedefs
  type RecordData = Array[Double]
  type Label = Double
}

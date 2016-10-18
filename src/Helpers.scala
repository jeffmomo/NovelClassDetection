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

  class OUTTH(initial: Double = 0.7, epsilon: Double, comparisonEpsilon: Double, clamp: Boolean = true) {

    var value = initial
    var eps = epsilon

    val compEps = comparisonEpsilon

    def adjustFalseNovel(weight: Double): Unit = {
      assert(weight < value)

      // increase slack space
      if(value - weight < comparisonEpsilon)
        value -= epsilon
    }


    def adjustFalseExisting(): Unit = {
      value += eps

      if(clamp && value > 1)
        value = 1
    }
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


  def qSNC(q: Int, x:Record, outliers: Array[Record], existing: Array[Record])(implicit metric:DistanceMetric): Double = {

    assert(outliers.length >= q && existing.length >= q)
//    assert(outliers.length + existing.length >= 1)

    val outlierDistance = getQMeanDistance(q, x, outliers)
    val existingClassDistance = getQMeanDistance(q, x, existing)

    (existingClassDistance - outlierDistance) / (existingClassDistance max outlierDistance)
  }

  private def getQMeanDistance(q:Int, x:Record, records:Array[Record])(implicit metric:DistanceMetric): Double = {
    val distances = records.map((a) => metric.distance(x.data, a.data)).sorted

    (for(i <- 0 until (q min records.length)) yield distances(i)).sum / q
  }


  type RecordData = Array[Double]
  type Label = Double

  implicit class AugmentedRecords(v: IndexedSeq[RecordData]) {

  }

}

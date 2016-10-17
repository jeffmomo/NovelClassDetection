import Helpers._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by jeffmo on 16/10/16.
  */





case class Cluster(radius: Double, centroid: Record)(implicit metric:DistanceMetric) {

  def contains(record: Record)(implicit outth: Double): Boolean = {
    val dist = distance(record)

    if (radius >= dist)
      true
    else
      Math.exp(radius - dist) >= outth
  }


  def distance(record: Record): Double = {
    val dist = metric.distance(centroid, record)

    if(dist > radius) dist else -1
  }
}




case class Classifier(clusters: Array[Cluster]) {
  def contains(x: Record): Boolean = {
    clusters.exists(_.contains(x))
  }
}

class Ensemble(maxClassifiers: Int = 10) {

  val classifiers = new ArrayBuffer[Classifier]()

  def addClassifier(x: Classifier): Unit = {

    // delete earliest classifier.
    if(classifiers.length + 1 > maxClassifiers)
      classifiers.remove(0)

    classifiers.append(x)
  }

  def contains(x: Record): Unit = {
    classifiers.exists(_.contains(x))
  }
}



class Centroid(initial: Record) {

  var mean = initial

  val records = new ArrayBuffer[Record]()

  def addRecord(record:Record): Unit = {
    records += record
  }

  def clearRecords(): Unit = {
    records.clear()
  }

  def calculateMean(): Record = {

    assert(records.nonEmpty)

    val dimensions = records(0).length
    val output = Array.fill[Double](dimensions)(0)

    records.foreach((a) => {
      for(i <- a.indices)
        output(i) += a(i)
    })

    mean = output.map(_ / records.length)
    mean
  }

}




object Clusterers {

  def assignRecordToCentroid(record: Record, centroids: Array[Centroid], metric:DistanceMetric): Int = {
    val distances = centroids.map((a) => metric.distance(a.mean, record))

    var maxIdx = 0
    var maxVal = distances(0)

    for(i <- 1 until distances.length) {
      if(distances(i) > maxVal) {
        maxIdx = i
        maxVal = distances(i)
      }
    }

    centroids(maxIdx).addRecord(record)
    maxIdx
  }

  def KMeans(records: Array[Record], k: Int, maxIterations: Int = 1000)(implicit distanceMetric: DistanceMetric = EuclideanDistance): Seq[Cluster] = {

    assert(records.length > 0)
    val dimension = records(0).length

    val fillerFn = Array.fill[Double](dimension)(_)

    val minArray = fillerFn(Double.MaxValue)
    val maxArray = fillerFn(Double.MinValue)

    for (record <- records) {
      for (i <- minArray.indices) {
        minArray(i) = Math.min(minArray(i), record(i))
        maxArray(i) = Math.max(maxArray(i), record(i))
      }
    }

    val centroids: Array[Centroid] =
      Array.range(0, k) map { _ => new Centroid(Array.range(0, dimension) map { i => Random.nextDouble() * (maxArray(i) + 1) - minArray(i) }) }

    var iterations = 0
    while(iterations < maxIterations) {
      centroids.foreach((a) => a.clearRecords())
      records.foreach((a) => assignRecordToCentroid(a, centroids, distanceMetric))
      centroids.foreach((a) => a.calculateMean())
      // Maybe add stopping when changes are minimal
      iterations += 1
    }

    return centroids.map((a) => {
      var maxDist = 0.0
      a.records.foreach((r) => maxDist = Math.max(maxDist, distanceMetric.distance(a.mean, r)))

      Cluster(maxDist, a.mean)
    })


  }


}

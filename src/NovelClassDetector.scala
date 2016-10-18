import Classifiers.Ensemble
import Helpers.{DistanceMetric, EuclideanDistance, _}
import RecordReader.Record

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import util.control.Breaks._

//noinspection SpellCheckingInspection
class NovelClassDetector(ensemble: Ensemble, k: Int = 50, chunkSize: Int = 1000, q: Int = 50)(implicit outth:OUTTH) {

  implicit val metric: DistanceMetric = EuclideanDistance

  val numIntervals: Int = 10
  val scThreshold: Double = 0.8
  val floatingpointEpsilon = 0.001


  type TupleOf_RecordData_Weight_qSNC = (Record, Double, Double)


  def partitionRealOutliers(f_outlierTuple: Array[(Record, Double)], classifier: Ensemble): (Array[Record], Array[Record]) = {

//    val ensembleLabels = ensemble.getLabels()
//    val (inlierTuple, f_outlierTuple) =
//      inputs
//        .map((a) => {
//
//
//          val output = (a, ensemble.weight(a.data))
//
//          if(ensembleLabels.contains(output._1.label))
//          {
//            if(output._2 < outth.value)
//              outth.adjustFalseNovel(output._2)
//          } else {
//            if(output._2 > outth.value)
//              outth.adjustFalseExisting()
//          }
//
//          output
//        })
//        .partition((a) => a._2 > 1)

    val (f_outliers, f_outlierWeights) = f_outlierTuple.unzip

    val (realOutliers: Array[TupleOf_RecordData_Weight_qSNC], filteredOuts) =
      f_outlierTuple
        .map((outlier) => (outlier._1, outlier._2, classifier.classifiers.map((clsf) => Helpers.qSNC(q, outlier._1, f_outliers, clsf.records)).min))
        .partition((tuple3) => tuple3._3 >= 0)


    //    val (inliers, _) = inlierTuple.unzip

    // assert forall qSNC, qSNC >= 0 && <= 1

    // add the filtered out ones to the inlier list
    val inlierBuffer = new ArrayBuffer[Record]() ++= filteredOuts.map(_._1)


//    val (realOutliers: Array[TupleOf_RecordData_Weight_qSNC], filteredOuts) =
//      f_outlierTuple
//        .map((a) => (a._1, a._2, Helpers.qSNC(q, a._1, f_outliers, inliers)))
//        .partition((tuple3) => tuple3._3 >= 0)



    assert(realOutliers.length + inlierBuffer.length == f_outlierTuple.length)

    if(realOutliers.isEmpty)
      return (inlierBuffer.toArray, new Array[Record](0))

    val minWeight = realOutliers.minBy((a) => a._2)._2
    val maxWeight = realOutliers.maxBy(a => a._2)._2


    // no need to sort actually
    val outliersWithNscoreOrdered = realOutliers.map((a) => (a._1, (maxWeight - a._2) * a._3 / (maxWeight - minWeight))).sortBy((a) => a._2)

    assert(outliersWithNscoreOrdered.forall((v) => v._2 >= 0 && v._2 <= 1))

    // Binning from min to max? or from 0 to 1
    val minNscore = 0  // outliersWithNscoreOrdered(0)._2
    val maxNscore = 1 + floatingpointEpsilon  // outliersWithNscoreOrdered.last._2
    val interval = (maxNscore - minNscore) / numIntervals.toDouble

    val bins = Array.fill[Int](numIntervals)(0)

    // need to make sure it is correctly filled
    outliersWithNscoreOrdered.foreach((a) => bins(((a._2 - minNscore) / interval).toInt) += 1)

    val sum = bins.sum
    assert(sum == outliersWithNscoreOrdered.length) // make sure we didnt lose any records

    val probabilities = bins.map((a) => a / sum.toDouble)

    assert(Math.abs(probabilities.sum - 1) < floatingpointEpsilon) // make sure probabilities are correct

    var probSum = 0.0
    val cdf = for(i <- probabilities.indices) yield {
      probSum += probabilities(i)
      probSum
    }

    val gini = Helpers.gini(cdf)
    if(gini > 0) {
      if(gini <= (outliersWithNscoreOrdered.length - 1.0) / (3.0 * outliersWithNscoreOrdered.length)) {
        // make sure this is actually the last interval
        val (outs, ins) = (outliersWithNscoreOrdered partition ((a) => a._2 >= minNscore + interval))
        inlierBuffer ++= ins map (_._1)
        return (inlierBuffer.toArray, outs.map(_._1))

      } else {
        return (inlierBuffer.toArray, outliersWithNscoreOrdered map (_._1))
      }
    }

    // All items filtered out
    return (inlierBuffer.toArray, new Array[Record](0))

  }

  def classifyItems(items: Array[RecordData]): mutable.Set[Record] = {

    val output = new mutable.HashSet[Record]()
    val connectedComponentsCentroids = buildGraph(items).toArray.map(getCentroidDistance(_)._1)

    for(i <- items.indices) {
      val distances = connectedComponentsCentroids.indices.map((idx) => (idx, metric.distance(connectedComponentsCentroids(idx), items(i))))
      val sorted = distances.sortBy(_._2)

      assert(sorted.nonEmpty)

      output += Record(items(i), sorted.head._1)
    }

    output
  }


  private def buildGraph(items: Array[RecordData]): mutable.Set[mutable.Set[RecordData]] = {

    assert(items.length > 0)

    // Need to add 1 in so that we have at least 1 cluster
    val k_v = k * items.length / chunkSize + 1

    // change RecordData to Record with bogus label
    val clusters = Clusterers.KMeans(items.map(Record(_, -1.0)), k_v)

    val graph = new Graph(new mutable.HashSet[RecordData]() ++= clusters.map(_.centroid))

    // Only attempt to connect if we have more than 1 vertex
    if(clusters.length > 1)
      clusters.foreach((c) => {
        val sortedClusterDistanceTuple = clusters.map((a) => (a, metric.distance(c.centroid, a.centroid))).sortBy(_._2)
        val avgDist = sortedClusterDistanceTuple.map(_._2).sum / (sortedClusterDistanceTuple.length - 1.0) // length - 1 to get rid of the current cluster
        val closestCluster = sortedClusterDistanceTuple(1) // closest at index 1 because index 0 will be the current cluster as distance(a, a) == 0

        val sc = Helpers.silhouetteCoefficient(closestCluster._2, avgDist)

        if(sc < scThreshold) {
          graph.addEdge(c.centroid, closestCluster._1.centroid)
        }

      })

    var changed = true
    val connectedComponents = graph.findConnectedComponents()

    while(changed) {

      changed = false

      breakable {
        for (a <- connectedComponents; b <- connectedComponents if a != b && !changed) {
          val aMetrics = getCentroidDistance(a)
          val bMetrics = getCentroidDistance(b)

          if (aMetrics._2 + bMetrics._2 > 2 * metric.distance(aMetrics._1, bMetrics._1)) {
            changed = true

            a ++= (b)
            val success = connectedComponents.remove(b)
            assert(!connectedComponents.contains(b) && success)
            break
          }
        }
      }
    }

    connectedComponents
  }

  private def getCentroidDistance(component: mutable.Set[RecordData]): (RecordData, Double) = {


    val summedRecord = Array.fill[Double](component.last.length)(0)
    component.foreach((record) => {
      for(i <- summedRecord.indices) {
        summedRecord(i) += record(i)
      }
    })
    val globalCentroid = summedRecord.map((v) => v / summedRecord.length)

    val distances = component.map(metric.distance(_, globalCentroid))
    val meanDistance = distances.sum / distances.size

    (globalCentroid, meanDistance)
  }

}

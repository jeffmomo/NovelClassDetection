/**
  * Created by jeffmo on 17/10/16.
  */

import java.io.PrintWriter

import Classifiers.{Ensemble, KMeansClassifier}
import Helpers._
import RecordReader._

import scala.collection.mutable.ArrayBuffer

object Evaluator {

  def evaluateEnsemble(records: Array[Record], classifier: Ensemble, chunkSize: Int, initialEnsembleSize: Int, novelClassThreshold:Int = 50, q: Int = 50, k: Int = 50, kmeansIters: Int)(implicit outth: OUTTH): Unit = {

    val printWriter = Some(new PrintWriter("output.txt"))

    var numTotal = 0
    var numCorrect = 0
    var novelDetected = 0
    var totalNovel = 0

    var TP = 0
    var FP = 0
    var FN = 0
    var TN = 0

    implicit val metric = EuclideanDistance

    val novelClassDetector = new NovelClassDetector(classifier, k, chunkSize, q, kmeansIters)


    val initialClassifiers = List.range(0, initialEnsembleSize).map((i) => {
      val datapointsForModel = records.slice(i * 1000, (i + 1) * 1000)
      KMeansClassifier(Clusterers.KMeans(datapointsForModel, k, kmeansIters), datapointsForModel)
    })
    initialClassifiers.foreach((c) => classifier.addClassifier(c))

    var start = initialClassifiers.length * 1000
    var end = start + 1000

    val f_outliersBuffer = new ArrayBuffer[(Record, Double)]()
    val outlierWasNovel = new scala.collection.mutable.HashMap[Record, Boolean]()

    while (start < records.length) {

      val currentChunk = records.slice(start, end)

      // here we do the streaming stuff.

      var chunkCorrect = 0
      var chunkTotal = 0
      val ensembleLabels = classifier.getLabels()

      for(sample <- currentChunk) {

        val weight = classifier.weight(sample.data)
        if(weight < outth.value) {
          // weight smaller than OUTTH, so must be novel

          f_outliersBuffer += Tuple2(sample, weight)
          outlierWasNovel.put(sample, !ensembleLabels.contains(sample.label))


          if(f_outliersBuffer.length >= novelClassThreshold) {

            val (inliers, outliers) = novelClassDetector.partitionRealOutliers(f_outliersBuffer.toArray, classifier)

            // classify inliers as with normal method
            for(sample <- inliers) {
              // classify the discarded outliers
              if (classifier.classify(sample.data) == sample.label)
                numCorrect += 1

              assert(outlierWasNovel.contains(sample))

              if(outlierWasNovel(sample)) {
                outth.adjustFalseExisting()
                // novel as existing
                FN += 1
              } else {
                // existing detected as existing
                TN += 1
              }
            }

            outliers.foreach((a) => {

              assert(outlierWasNovel.contains(a), "did not keep a novelness history for this particular instance???")

              if(outlierWasNovel(a)) {
                TP += 1
              } else {
                outth.adjustFalseNovel(0)
                FP += 1
              }
            })

            f_outliersBuffer.clear()
            outlierWasNovel.clear()

          }
        } else {

          chunkTotal += 1

          // adjust outth if sample is false existing
          // If novel class is not detected
          if(!ensembleLabels.contains(sample.label)) {
            outth.adjustFalseExisting()
            // novel as existing
            FN += 1
          } else {
            // existing detected as existing
            TN += 1
          }

          if(classifier.classify(sample.data) == sample.label)
            chunkCorrect += 1
        }
      }

      println("total novel", TP + FN, "detected as Novel", TP + FP)
      println("fraction falsely identified as existing", FN.toDouble / (TP + FN))
      println("fraction falsly identified as novel", FP.toDouble / (TN + FP))
      println("combined error", (FN + FP).toDouble / (FN + FP + TN + TP))

      printWriter match {
        case Some(p) => p.println(List(TP + FP, TP + FN, FN.toDouble / (TP + FN), FP.toDouble / (TN + FP)).mkString(","))
      }

      numCorrect += chunkCorrect
      numTotal += chunkTotal

      start += 1000
      end = (start + 1000) min records.length

      // Add a classifier trained on the new chunk of data
      classifier.addClassifier(KMeansClassifier(Clusterers.KMeans(currentChunk, k, kmeansIters), currentChunk))
    }
    println("Final results", numCorrect.toDouble / numTotal)

    // close printwriter
    printWriter.foreach(_.close())
  }

}

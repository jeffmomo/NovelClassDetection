/**
  * Created by jeffmo on 17/10/16.
  */

import Classifiers.{Ensemble, KMeansClassifier}
import Helpers._
import RecordReader._

import scala.collection.mutable.ArrayBuffer

object Evaluator {

  def evaluateEnsemble(records: Array[Record], classifier: Ensemble, chunkSize: Int, initialEnsembleSize: Int = 3, novelClassThreshold:Int = 200, q: Int = 50)(implicit outth: OUTTH): Unit = {

    var numTotal = 0
    var numCorrect = 0

    var novelDetected = 0
    var totalNovel = 0

    implicit val metric = EuclideanDistance

    val novelClassDetector = new NovelClassDetector(classifier)

    // do assertations

    val initialClassifiers = List.range(0, initialEnsembleSize).map((i) => {
      val datapointsForModel = records.slice(i * 1000, (i + 1) * 1000)
      KMeansClassifier(Clusterers.KMeans(datapointsForModel, 50), datapointsForModel)
    })
    initialClassifiers.foreach((c) => classifier.addClassifier(c))

    var start = initialClassifiers.length * 1000
    var end = start + 1000

    val f_outliersBuffer = new ArrayBuffer[(Record, Double)]()

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


          // adjust outth depending on if it is real novel
          if(ensembleLabels.contains(sample.label)) {
            outth.adjustFalseNovel(weight)
          }

          if(f_outliersBuffer.length >= novelClassThreshold) {
            // now we perform novel class detection

//            val q_count = f_outliersBuffer.count((outlier) => {
//              classifier.classifiers.forall((clsf) => Helpers.qSNC(q, outlier, f_outliersBuffer.toArray, clsf.records) >= 0)
//            })
//
//            if(q_count > q) {
//              novelDetected += f_outliersBuffer.length
//              println(s"Novel classes detected: ${f_outliersBuffer.length}")
//
//            } else {
//              println("novel classes discarded")
//
//              for(sample <- f_outliersBuffer) {
//                // classify the discarded outliers
//                if (classifier.classify(sample.data) == sample.label)
//                  numCorrect += 1
//              }
//
//            }

            val (inliers, outliers) = novelClassDetector.partitionRealOutliers(f_outliersBuffer.toArray, classifier)

            // classify inliers as with normal method
            for(sample <- inliers) {
              // classify the discarded outliers
              if (classifier.classify(sample.data) == sample.label)
                numCorrect += 1
            }
            if(outliers.nonEmpty) {
              val classified = novelClassDetector.classifyItems(outliers.map(_.data))
              println(s"Novel classes detected: ${classified.size}")
              novelDetected += classified.size
            }


            f_outliersBuffer.clear()

          }
        } else {
          chunkTotal += 1

          // adjust outth if sample is false existing
          if(!ensembleLabels.contains(sample.label)) {
            outth.adjustFalseExisting()
          }


          if(classifier.classify(sample.data) == sample.label)
            chunkCorrect += 1
        }

        if(!ensembleLabels.contains(sample.label)) {
          totalNovel += 1
        }

      }

      println("total novel", totalNovel, "detectedNovel", novelDetected)
      println("accuracy on chunk", chunkCorrect / chunkTotal.toDouble)
      println("total accuracy", numCorrect / numTotal.toDouble)

      numCorrect += chunkCorrect
      numTotal += chunkTotal






      //  end

//      val classifierSeenLabels = classifier.getLabels()
//      val groundTruthNovelCount = currentChunk.map(_.label).count((a) => !classifierSeenLabels.contains(a))
//
//
//
//
////      val (inliers, outliers) = currentChunk.par.partition((r) => classifier.contains(r.data))
//      val (inliers, n_list) = novelClassDetector.partitionRealOutliers(currentChunk)
//
//
//
//      val classified = if(n_list.length > 0) Some(novelClassDetector.classifyItems(n_list.map(_.data))) else None
//
//      if(n_list.length > 0) {
//        //println("Has novel")
//        val gotten = classified.get
//
//        if(gotten.size > groundTruthNovelCount) {
//
//          println("false novel detected", gotten.size, "true novel count:", groundTruthNovelCount)
//          val marginalFalseNovels = n_list.filter((a) => classifierSeenLabels.contains(a.label)).map((a) => classifier.weight(a.data))
//          //marginalFalseNovels.foreach((a) => outth.adjustFalseNovel(a))
//
//        }
//        else if(gotten.size < groundTruthNovelCount) {
//          println("fraction detected", gotten.size.toDouble / groundTruthNovelCount, gotten.size)
//          //(0 until groundTruthNovelCount - gotten.size).foreach((a) => outth.adjustFalseExisting())
//        }
//      } else {
//        if(groundTruthNovelCount > 0)
//          println("not detected at all:", groundTruthNovelCount)
//      }
//
//      val chunkCorrects = inliers.par.map((record) => {
//        val classified = classifier.classify(record.data)
//        val correct = record.label == classified
//
//        if (classifier.getLabels().contains(classified)) {
//
//        }
//
//        correct
//      })
//
//      val (correct, incorrect) = chunkCorrects.foldRight((0, 0))((a, accum) => if (a) (accum._1 + 1, accum._2) else (accum._1, accum._2 + 1))
//
//      val thisRoundAccuracy = correct.toDouble / (correct + incorrect)
//      println("This round:", thisRoundAccuracy)
//
//      numCorrect += correct
//      numTotal += correct + incorrect

      start += 1000
      end = (start + 1000) min records.length

      classifier.addClassifier(KMeansClassifier(Clusterers.KMeans(currentChunk, 50), currentChunk))
    }

    println("Final results", numCorrect.toDouble / numTotal)
  }

}

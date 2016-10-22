import Classifiers.Ensemble
import Helpers.{OUTTH, RecordData}
import RecordReader.Record

import scala.util.Try


object Main {
  def main(args: Array[String]): Unit = {
    // dataset=path [order=default|random|altitude_sorted] [normalize=true|false] [outth_clamp=true|false] kmeans_iters=Int ensemble_size=Int chunk_size=Int


    val argsMap = args.foldRight(new scala.collection.mutable.HashMap[String, String]())( {

      case ("", accum) => accum
      case (argString, accum) => {

        val splitted = argString.split("=")

        assert(splitted.length == 2, "arguments are badly formatted. Usage: scala -J-Xmx4g out.jar dataset=path [order=default|random|sorted_by_first_col] [normalize=true|false] [outth_clamp=false|true] [kmeans_iters=Integer] [ensemble_size=Integer] [chunk_size=Integer]")

        accum.put(splitted.head, splitted.last)

        accum
      }
    })

    if(argsMap.get("dataset").isEmpty) {
      println("Usage: scala -J-Xmx4g out.jar dataset=path [order=default|random|sorted_by_first_col] [normalize=true|false] [outth_clamp=false|true] [kmeans_iters=Integer] [ensemble_size=Integer] [chunk_size=Integer]")
      System.exit(-1)
    }


    def getNumericalArg(key:String, defaultVal: Double): Double = {
      Try({ argsMap.getOrElse(key, defaultVal.toString).toDouble })
        .toOption
        .getOrElse(defaultVal)
    }

    val records = RecordReader.getRecords(argsMap.getOrElse("dataset", "covtype.data"))
    assert(records.length > 0, "records must contain at least one line")

    if(argsMap.getOrElse("normalize", "true") == "true")
      normalise_mutable(records)

    val reordered = argsMap.get("order") match {
      case Some("default") => records
      case Some("random") => new DepletingRandomiser(records.length).map((idx) => records(idx)).toArray
      case Some("sorted_by_first_col") => records.sortBy((a) => a.data(0))
      case _ => records
    }

    implicit val outth = new OUTTH(0.7, 0.02, 10, argsMap.getOrElse("outth_clamp", "false") == "true")
    Evaluator.evaluateEnsemble(reordered, new Ensemble(getNumericalArg("ensemble_size", 6).toInt), getNumericalArg("chunk_size", 1000).toInt, 3, kmeansIters = getNumericalArg("kmeans_iters", 10).toInt)

  }

  // normalises given recrods to [0, 1]
  def normalise_mutable(records: Array[Record]): Unit = {
    val dimensions = records(0).data.length

    val (mins, maxes) = records.foldRight((new RecordData(dimensions), new RecordData(dimensions)))((a, accum) => {
      val (mins, maxes) = accum
      for (i <- a.data.indices) {
        mins(i) = Math.min(mins(i), a.data(i))
        maxes(i) = Math.max(maxes(i), a.data(i))
      }
      (mins, maxes)
    })
    val ranges = for (i <- mins.indices) yield maxes(i) - mins(i)

    // normalise records
    records.par.foreach({
      case Record(d, l) => {
        for (i <- d.indices if ranges(i) != 0) {
          d(i) = (d(i) - mins(i)) / ranges(i)
          assert(d(i) <= 1 && d(i) >= 0)
        }
      }
    })
  }
}

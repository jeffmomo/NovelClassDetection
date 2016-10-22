/**
  * Created by jeffmo on 16/10/16.
  */

import scala.util.Random

// Returns a random sequence from a pool of Ints up to the max value
// Used for randomising the order of records
class DepletingRandomiser(max: Int) extends Traversable[Int] {

  override def foreach[U](f: (Int) => U): Unit = {
    val baseArray = new Array[Int](max)
    var count = max

    for(a <- 0 until max)
      baseArray(a) = a

    while(count > 0) {
      val rnd = Random.nextInt(count)
      count -= 1
      f(baseArray(rnd))
      baseArray(rnd) = baseArray(count)
    }
  }


}

package deepSRL.utils

/**
  * Created by seb on 08.12.15.
  */
object Histogram {

  /*def create(listOne: List[List[String]], listTwo: List[List[String]], normalize: Boolean = true) = {
    val distinctValue = (listOne ++ listTwo).distinct

    if (normalize) {
      (normalizeByArea(distinctValue.map( x => listOne.count( _ == x ))), normalizeByArea(distinctValue.map( x => listTwo.count( _ == x))))
    }
    else {
      (distinctValue.map( x => listOne.count( _ == x ).toDouble), distinctValue.map( x => listTwo.count( _ == x).toDouble))
    }
  }*/

  def create(listOne: List[String], listTwo: List[String], normalize: Boolean = true) = {
    val distinctValue = (listOne ++ listTwo).distinct

    if (normalize) {
      (normalizeBySum(distinctValue.map( x => listOne.count( _ == x ))).map( x => if (x.isNaN) 0.0 else x), normalizeBySum(distinctValue.map( x => listTwo.count( _ == x))).map( x => if (x.isNaN) 0.0 else x))
    }
    else {
      (distinctValue.map( x => listOne.count( _ == x ).toDouble).map( x => if (x.isNaN) 0.0 else x), distinctValue.map( x => listTwo.count( _ == x).toDouble).map( x => if (x.isNaN) 0.0 else x))
    }
  }

  def normalizeByArea(histogram: List[Int]) = {
    val normConstant = trapezoidIntegration(histogram)
    histogram.map( _.toDouble / normConstant)
  }

  def normalizeBySum(histogram: List[Int]) = {
    val normConstant = histogram.sum
    histogram.map( _.toDouble / normConstant)
  }

  private def trapezoidIntegration(samples: List[Int]) = {
    //TODO: check if this is a proper way of calculating it; refer to Wikipedia article
    0.5 * (samples.head + samples.slice(1, samples.length - 1).map( _*2.toDouble).sum + samples.last)
  }

}

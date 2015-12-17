package deepSRL.utils

/**
  * Created by seb on 08.12.15.
  */
object HistogramDistance {

  val minError = 0.001

  def manhattan(histOne: List[Double], histTwo: List[Double]) = {
    histOne.zip(histTwo).map( x => math.abs(x._1 - x._2)).sum
  }

  def euclidean(histOne: List[Double], histTwo: List[Double]) = {
    math.sqrt(histOne.zip(histTwo).map( x => math.pow(x._1 - x._2, 2.0)).sum)
  }

  def chiSquared(histOne: List[Double], histTwo: List[Double]) = {
    0.5 * histOne.zip(histTwo).map( x => math.pow(x._1 - x._2, 2.0)/(x._1 + x._2)).sum
  }

  def kolmogorovSmirnov(histOne: List[Double], histTwo: List[Double]) = {
    histOne.zip(histTwo).map( x => math.abs(x._1 - x._2)).max
  }

  def kullbackLeibler(histOne: List[Double], histTwo: List[Double]) = {
    smooth(histOne).zip(smooth(histTwo)).map(x => x._1 * math.log(x._1/x._2)).sum
  }

  def jeffrey(histOne: List[Double], histTwo: List[Double]) = {
    smooth(histOne).zip(smooth(histTwo)).map(x => {val m = (x._1 + x._2)/2; x._1 * math.log(x._1/m) + x._2 * math.log(x._2/m)}).sum
  }

  def smooth(hist: List[Double]) = {
    hist.map( x => if(x == 0.0) { minError} else x )
  }

}

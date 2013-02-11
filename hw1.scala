/* Richard Hwang*/
/* CS294-1 */
/* Spring 2013 */

import scala.io._
import scala.sys.process._
import scala.math

object NaiveBayes {

  val pos_example_dir =
    "/Users/richard/classes/294-1/hw1/review_polarity/txt_sentoken/pos"

  /* Returns a sparse matrix of features*/
  def process() {
    // First create dictionary, term->index

    // Then create doc, word matrix
    val pos_file_names = "ls %s".format(pos_example_dir).split("\n")
    for (i <- 0 until pos_file_names.length) {
      val s = Source.fromFile(pos_file_names(i))
      s.getLines.foreach( (line) => {
        // TODO Fill me in
      })
  }
  }

  /* Trains a classifer, ie computing log(P(t|c)).
   * Returns log(P(t|c)) for all t and c. */
  def train(doc_fmat: Array[Array[Array[Int]]]): Array[Array[Double]] = {
    /* Takes in |V| x |D| matrix doc_fmat of DOCument term Frequencies MATrix*/
    val pos_doc_fmat = doc_fmat(0)  //positive documents
    val neg_doc_fmat = doc_fmat(1)  //negative documents
    val num_features = pos_doc_fmat.length
    val num_docs = pos_doc_fmat(0).length
    /* Compile into a |V| x 2 matrix fmat of aggregate term freuquencies */
    val pos_freq = new Array[Double](num_features)
    val neg_freq = new Array[Double](num_features)
    var pos_sum = 0.0
    var neg_sum = 0.0
    for (i <- 0 until num_features) {
      var pos_feature = 0.0
      var neg_feature = 0.0
      for (j <- 0 until num_docs) {
        pos_feature = pos_feature + pos_doc_fmat(i)(j).toDouble
        neg_feature = neg_feature + neg_doc_fmat(i)(j).toDouble
      }
      pos_freq(i) = pos_feature
      pos_sum = pos_sum + pos_feature
      neg_freq(i) = neg_feature
      neg_sum = neg_sum + neg_feature
    }
    /* Construct probability matrix log(P(t|c)) */
    var pos_prob = new Array[Double](num_features)
    var neg_prob = new Array[Double](num_features)
    for (i <- 0 until num_features) {
      pos_prob(i) = math.log( pos_freq(i) / pos_sum )
      neg_prob(i) = math.log( neg_freq(i) / neg_sum )
    }
    val pmat = Array(pos_prob, neg_prob)
    return pmat
  }

  /* Performs 10-fold cross-validation, and applies an accuracy measure. */
  def validation() {
  }

  def main(args: Array[String]) {
    val features = process()
  }

}

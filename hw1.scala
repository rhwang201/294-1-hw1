/* Richard Hwang*/
/* CS294-1 */
/* Spring 2013 */

import scala.io._
import scala.sys.process._
import scala.collection.mutable
import scala.math

object NaiveBayes {

  val example_dir =
    "/Users/richard/classes/294-1/hw1/review_polarity/txt_sentoken/"

  /* Returns a dictionary of term->index*/
  def create_dict(): mutable.Map[String,Int]  = {
    val term_index = mutable.Map.empty[String, Int]
    var i = 0

    var files = "ls %spos".format(example_dir).!!.split("\n")
    files.foreach( (file: String) => {
      val s = Source.fromFile(example_dir + "pos/" + file)
      s.getLines.foreach( (line: String) => {
          line.split("[\\s.,();:!?&\"]+").foreach({ (word: String) =>
            if (!term_index.keySet.exists(_ == word)) {
              term_index(word) = i
              i += 1
            }
          })
      })
    })
    println("Finished positive examples")

    files = "ls %sneg".format(example_dir).!!.split("\n")
    files.foreach( (file: String) => {
      val s = Source.fromFile(example_dir + "neg/" + file)
      s.getLines.foreach( (line: String) => {
          line.split("[\\s.,();:!?&\"]+").foreach({ (word: String) =>
            if (!term_index.keySet.exists(_ == word)) {
              term_index(word) = i
              i += 1
            }
          })
      })
    })

    return term_index
  }

  /* Returns a sparse matrix of features*/
  def process() = {
    val termDict = create_dict()

    // Then create doc, word matrix
    val pos_file_names = "ls %sneg".format(example_dir).split("\n")
    for (i <- 0 until pos_file_names.length) {
      val s = Source.fromFile(pos_file_names(i))
      s.getLines.foreach( (line) => {
        // TODO Fill me in
      })
  }
  }

  /* Trains a classifer, ie computing log(P(t|c)).
   * Returns log(P(t|c)) for all t and c. */
  def train() {
    /* Takes in |V| x |D| matrix doc_fmat of document term frequencies */
    //num_features = doc_fmat(0).length
    //val pos_freq = fmat(0)
    //val neg_freq = fmat(1)
    //var pos_sum = 0
    //var neg_sum = 0
    //for (i <- 0 until num_features) {
    //  pos_sum = pos_sum + pos_featuers(i)
    //  neg_sum = neg_sum + neg_features(i)
    //}
    ///* Construct probability matrix log(P(t|c)) */
    //var pos_prob = new Array[Double](num_features)
    //var neg_prob = new Array[Double](num_features)
    //for (i <- 0 until num_features) {
    //  pos_prob(i) = log( pos_freq(i) / pos_sum )
    //  neg_prob(i) = log( neg_freq(i) / neg_sum )
    //}
    //val pmat = Array(pos_prob, neg_prob)
    //return pmat
  }

  /* Performs 10-fold cross-validation, and applies an accuracy measure. */
  def validation(logs: Array[Array[Double]]) = {
  }

  def main(args: Array[String]) = {
    val index = create_dict()
    print("test")
  }

}

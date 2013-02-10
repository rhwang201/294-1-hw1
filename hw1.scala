/* Richard Hwang*/
/* CS294-1 */
/* Spring 2013 */

import scala.io._
import scala.sys.process._

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
  def train() {
    /* comment*/
  }

  /* Performs 10-fold cross-validation, and applies an accuracy measure. */
  def validation() {
  }

  def main(args: Array[String]) {
    val features = process()
  }

}

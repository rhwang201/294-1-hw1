/* Richard Hwang*/
/* CS294-1 */
/* Spring 2013 */

import scala.io._
import scala.sys.process._
import scala.collection.mutable

object NaiveBayes {

  val example_dir =
    "/Users/richard/classes/294-1/hw1/review_polarity/txt_sentoken/"

  /* Returns a dictionary of term->index*/
  def create_dict() = {
    term_index = mutable.Map.empty[String, Int]
    var i = 0

    "ls %spos".format(example_dir).split("\n").foreach( (i) -> {
      val s = Source.fromFile(pos_file_names(i))
      s.getLines.foreach( (line: String) => {
          line.split("[\\s.,();:!?&\"]+").foreach({ (word: String) =>
            if (!term_index.keySet.exists(_ == word)) {
              term_index(word) = i
              i += 1
            }
          })
      })
    })

    "ls %sneg".format(example_dir).split("\n").foreach( (i) -> {
      val s = Source.fromFile(pos_file_names(i))
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
  def train() = {
    /* comment*/
  }

  /* Performs 10-fold cross-validation, and applies an accuracy measure. */
  def validation(logs: Array[Array[Double]]) = {
  }

  def main(args: Array[String]) = {
    val index = create_dict()
    print("test")
  }

}

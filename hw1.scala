/* Richard Hwang*/
/* CS294-1 */
/* Spring 2013 */

import scala.io._
import scala.sys.process._
import scala.collection.mutable
import scala.math
import java.io._

object NaiveBayes {

  val example_dir =
    "/Users/richard/classes/294-1/hw1/review_polarity/txt_sentoken/"

  val term_index_filename = "term_index.txt"

  /* Useful. */
  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  /* Writes a dictionary of term->index to a file. */
  def create_dict(): mutable.Map[String,Int]  = {
    val term_index = mutable.Map.empty[String, Int]
    var i = 0

    var files = "ls %spos".format(example_dir).!!.split("\n")
    files.par.foreach( (file: String) => {
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
    println("Finished indexing positive examples")

    files = "ls %sneg".format(example_dir).!!.split("\n")
    files.par.foreach( (file: String) => {
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
    println("Finished indexing negative examples")

    // Writing results to file
    printToFile(new File(term_index_filename))(p => {
      term_index.foreach(t => {
        p.println(t._1 + "," + t._2)
      })
    })
    println("Wrote to " + term_index_filename)

    return term_index;
  }

  /* Returns a sparse matrix of features*/
  def process(read_index: Boolean = False) = {
    if (read_index) {
      val term_index = mutable.Map.empty[String, Int]
      val s = Source.fromFile(term_index_filename)
      s.getLines.foreach( (line: String) => {
        line.split(",") match {
          case Array(str, num) => { term_index(str) = num.toInt }
          case _ => error("too many commas")
        }
      })
    } else {
      val term_index = create_dict()
    }

    // NOTE: do the rest from within BIDMat????

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
  def train(doc_fmat: Array[Array[Array[Int]]]): Array[Array[Double]] = { //Takes in Integer frequency matrix; outputs Double log-probability matrix
    /* Takes in |V| x |D| matrix doc_fmat of DOCument feature-Frequency MATrix*/
    val pos_doc_fmat = doc_fmat(0)  //positive documents
    val neg_doc_fmat = doc_fmat(1)  //negative documents
    val num_features = pos_doc_fmat.length  //number of rows/features
    val num_docs = pos_doc_fmat(0).length //number of columns/documents
    /* Compile into two |V| x 1 matrices pos/neg_freq of aggregate feature-Frequency MATrix */
    val pos_freq = new Array[Double](num_features)  //positive feature frequency matrix
    val neg_freq = new Array[Double](num_features)  //negative feature frequency matrix
    var pos_sum = 0.0 //total word count in positive documents
    var neg_sum = 0.0 //total word count in negative documents
    for (i <- 0 until num_features) { //iterate through rows/features
      var pos_feature = 0.0 //word count for a particular word in positive documents
      var neg_feature = 0.0 //word count for a particular word in negative documents
      for (j <- 0 until num_docs) { //iterate through columns/documents
        pos_feature = pos_feature + pos_doc_fmat(i)(j).toDouble //aggregate the same feature across all positive documents
        neg_feature = neg_feature + neg_doc_fmat(i)(j).toDouble //aggregate the same feature across all negative documents
      }
      pos_freq(i) = pos_feature //populate i_th row/feature in positive document frequency matrix
      pos_sum = pos_sum + pos_feature //augment total word count in positive documents
      neg_freq(i) = neg_feature //populate i_th row/feature in negative document frequency matrix
      neg_sum = neg_sum + neg_feature //augment total word count in negative documents
    }
    /* Construct two probability matrices for log(P(t|c)) counterpart of frequency entries */
    var pos_prob = new Array[Double](num_features)  //positive feature log-probability matrix
    var neg_prob = new Array[Double](num_features)  //negative feature log-probability matrix
    for (i <- 0 until num_features) { //iterate through rows/features
      pos_prob(i) = math.log( pos_freq(i) / pos_sum ) //compute & populate i_th row/feature in positive log-probability matrix
      neg_prob(i) = math.log( neg_freq(i) / neg_sum ) //compute & populate i_th row/feature in negative log-probability matrix
    }
    val pmat = Array(pos_prob, neg_prob)  //combine positive & negative log-probability matrix into one Probability MATrix
    return pmat  //note that output log-probabilities are negative (i.e. more frequent terms show up as less frequent terms => take absolute value before further computation?)
  }

  /* Performs 10-fold cross-validation, and applies an accuracy measure. */
  def validation(logs: Array[Array[Double]]) = {
  }

  def main(args: Array[String]) = {
    var term_index = create_dict()
  }

}

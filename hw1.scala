/* Richard Hwang and David Huang */
/* CS294-1 */
/* Spring 2013 */

/* INSTRUCTIONS:
 *   To run this script with BIDMat, place this file in
 *   ${BIDMat}/src/main/scala. To compile, run "sbt compile" from project
 *   root. Class files will be in target/scala-2.9.2/classes. From there, you
 *   can run the typical "scala NaiveBayes" command.
 */

import scala.io._
import scala.sys.process._
import scala.collection.mutable
import scala.math
import java.io._
import java.util.StringTokenizer
import BIDMat.{Mat, FMat, DMat, IMat, CMat, BMat, CSMat, SMat, SDMat, GMat, GIMat, GSMat, HMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._
import BIDMat.Solvers._
import BIDMat.Plotting._

object NaiveBayes {

  val example_dir =
    "/Users/richard/classes/294-1/hw1/review_polarity/txt_sentoken/"

  val term_index_dir = "/Users/richard/classes/294-1/hw1/"
  val term_index_filename = "term_index.txt"
  val pos_files = "ls %spos".format(example_dir).!!.split("\n")
  val neg_files = "ls %sneg".format(example_dir).!!.split("\n")

  val word_mat_dir = "/Users/richard/classes/294-1/BIDMat/"
  val word_mat_name = "words.mat"

  val num_documents = 2000

  val priors = Array(0.5, 0.5)
  val test_set_size = 100

  /* Useful. */
  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  /* Writes a dictionary of term->index to a file. */
  def create_dict(): mutable.Map[String,Int]  = {
    val term_index = mutable.Map.empty[String, Int]
    var i = 0

    pos_files.par.foreach( (file: String) => {
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

    neg_files.par.foreach( (file: String) => {
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

  /* Writes a dictionary of term->index to a file. */
  def create_dict_for(): mutable.Map[String,Int]  = {
    val term_index = mutable.Map.empty[String, Int]
    var z = 0

    var files = "ls %spos".format(example_dir).!!.split("\n")
    for (i <- 0 until files.length) {
      val s = Source.fromFile(example_dir + "pos/" + files(i))
      s.getLines.foreach( (line: String) => {
        var words = line.split("[\\s.,();:!?&\"]+")
        for (k <- 0 until words.length) {
          var word = words(k)
          if (!term_index.keySet.exists(_ == word)) {
            term_index(word) = z
            z += 1
          }
        }
      })
    }
    println("Finished indexing positive examples")

    files = "ls %sneg".format(example_dir).!!.split("\n")
    files.par.foreach( (file: String) => {
      val s = Source.fromFile(example_dir + "neg/" + file)
      s.getLines.foreach( (line: String) => {
          line.split("[\\s.,();:!?&\"]+").foreach({ (word: String) =>
            if (!term_index.keySet.exists(_ == word)) {
              term_index(word) = z
              z += 1
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
  def process(read_index: Boolean = false): BIDMat.SMat = {
    // Get term_index
    var term_index = mutable.Map.empty[String, Int]
    if (read_index) {
      val s = Source.fromFile(term_index_dir + term_index_filename)
      s.getLines.foreach( (line: String) => {
        line.split(",") match {
          case Array(str, num) => { term_index(str) = num.toInt }
          case _ => sys.error("too many commas")
        }
      })
    } else {
      term_index = create_dict()
    }

    // Construct words_docs matrix
    val num_words = term_index.size

    Mat.noMKL = true
    var words_docs: FMat = zeros(num_words, num_documents)

    for (i <- 0 until pos_files.length) {
      val s = Source.fromFile(example_dir + "pos/" + pos_files(i))
      s.getLines.foreach( (line) => {
        line.split("[\\s.,();:!?&\"]+").foreach({ (word: String) =>
          words_docs(term_index(word), i) += 1
        })
      })
    }
    for (i <- 0 until neg_files.length) {
      val s = Source.fromFile(example_dir + "neg/" + neg_files(i))
      s.getLines.foreach( (line) => {
        line.split("[\\s.,();:!?&\"]+").foreach({ (word: String) =>
          words_docs(term_index(word), i) += 1
        })
      })
    }

    var sparse_words_docs: SMat = sparse(words_docs)
    saveAs(word_mat_dir + word_mat_name, sparse_words_docs, "words_docs")
    return sparse_words_docs
  }

  /* TODO */
  def file_to_vect() = {
  }

  /* Trains a classifer, ie computing log(P(t|c)) for all t and c. */
  //def train(docs_fmat: Array[Array[Array[Int]]]): Array[Array[Double]] = { //Takes in Integer frequency matrix; outputs Double log-probability matrix
  //  /* Takes in |V| x |D| matrix doc_fmat of DOCument feature-Frequency MATrix*/
  //  val pos_docs_fmat = docs_fmat(0)  //positive documents
  //  val neg_docs_fmat = docs_fmat(1)  //negative documents
  //  val num_features = pos_docs_fmat.length  //number of rows/features
  //  val num_docs = pos_docs_fmat(0).length //number of columns/documents
  //  /* Compile into two |V| x 1 matrices pos/neg_freq of aggregate feature-Frequency MATrix */
  //  val pos_freq = new Array[Double](num_features)  //positive feature frequency matrix
  //  val neg_freq = new Array[Double](num_features)  //negative feature frequency matrix
  //  var pos_sum = 0.0 //total word count in positive documents
  //  var neg_sum = 0.0 //total word count in negative documents
  //  for (i <- 0 until num_features) { //iterate through rows/features
  //    var pos_feature = 0.0 //word count for a particular word in positive documents
  //    var neg_feature = 0.0 //word count for a particular word in negative documents
  //    for (j <- 0 until num_docs) { //iterate through columns/documents
  //      pos_feature += pos_doc_fmat(i)(j).toDouble //aggregate the same feature across all positive documents
  //      neg_feature += neg_doc_fmat(i)(j).toDouble //aggregate the same feature across all negative documents
  //    }
  //    pos_freq(i) = pos_feature //populate i_th row/feature in positive document frequency matrix
  //    pos_sum += pos_feature //augment total word count in positive documents
  //    neg_freq(i) = neg_feature //populate i_th row/feature in negative document frequency matrix
  //    neg_sum += neg_feature //augment total word count in negative documents
  //  }
  //  /* Construct two probability matrices for log(P(t|c)) counterpart of frequency entries */
  //  var pos_prob = new Array[Double](num_features)  //positive feature log-probability matrix
  //  var neg_prob = new Array[Double](num_features)  //negative feature log-probability matrix
  //  for (i <- 0 until num_features) { //iterate through rows/features
  //    pos_prob(i) = math.log( pos_freq(i) / pos_sum ) //compute & populate i_th row/feature in positive log-probability matrix
  //    neg_prob(i) = math.log( neg_freq(i) / neg_sum ) //compute & populate i_th row/feature in negative log-probability matrix
  //  }
  //  val pmat = Array(pos_prob, neg_prob)  //combine positive & negative log-probability matrix into one Probability MATrix
  //  return pmat  //note that output log-probabilities are negative (i.e. more frequent terms show up as less frequent terms => take absolute value before further computation?)
  //}

  /* Classifies the sentiment level of a given document. */
  def classify(model: Array[Array[Double]], priors: Array[Double], doc: Array[Int]): Int = {
    /* TODO: Process given document into its sparse-matrix, log-probability representation */

    val num_features = model(0).length
    var pmat = Array[Double](num_features)
    /* Compute Maximum A-Posteriori (MAP) estimate for positive/negative sentiment */
    val pos_model = model(0)
    var pos_map = math.log(priors(0))
    val neg_model = model(1)
    var neg_map = math.log(priors(1))
    for (i <- 0 until num_features) {
      pos_map += ( doc(i) * pos_model(i) )
      neg_map += ( doc(i) * neg_model(i) )
    }
    println(pos_map)
    println(neg_map)
    /* Output a sentiment level. */
    var sent = 100  //a distinctly non-appropriate value
    if (pos_map >= neg_map) {
      sent = 1
    } else {
      sent = -1
    }
    return sent
  }

  /* Performs 10-fold cross-validation, and applies an accuracy measure. */
  //def validation(docs: String): Array[Double] = {
  //  //PSEUDO-CODE//
  //  /* Loop 10 times; segregate training set from test set. */
  //  var test_set = 
  //  var training_set = 
  //  var label = //1 or -1
  //  var result = 0
  //  var set_correct = 0
  //  var set_accuracy
  //  var best_accuracy = 0.0
  //  var best_other_measure = 0.0
  //  var best_set = 0
  //  for (i <- 0 until 10) {
  //    test_set = docs(i)
  //    training_set = docs - doc(i)
  //    /* Process into appropriate matrices & compute model. */
  //    val test_mat = 
  //    val training_mat = 
  //    val model = train(training_mat)
  //    /* Classify & evaluate */
  //    for (i <- 0 until test_set_size) {
  //      result = classify(model, priors, test_mat(i))
  //      if (result == label) {
  //        set_correct+=1
  //      }
  //      set_accuracy = set_correct / test_set_size
  //    }
  //    if (set_accuracy > best_accuracy) {
  //      best_accuracy = set_accuracy
  //      best_set = i
  //    }
  //  }
  //  /* Determine best model. */
  //  return Array(best_accuracy, best_other_measure)
  //}

  def main(args: Array[String]) = {
    process(true)
  }
}

/* Richard Hwang and David Huang */
/* CS294-1 */
/* Spring 2013 */

/* INSTRUCTIONS:
 *   To run this script with BIDMat, place this file in
 *   ${BIDMat}/src/main/scala. To compile, run "sbt compile" from project
 *   root. Class files will be in target/scala-2.9.2/classes. From there, you
 *   can run:
 *     scala -J-Xmx2g NaiveBayes
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
  val david_example_dir =
    "/Users/Davidius/294-1-hw1/review_polarity/txt_sentoken/"

  val term_index_dir = "/Users/richard/classes/294-1/hw1/"
  val david_term_index_dir = "/Users/Davidius/294-1-hw1/"
  val term_index_filename = "term_index.txt"
  val pos_files = "ls %spos".format(example_dir).!!.split("\n")
  val neg_files = "ls %sneg".format(example_dir).!!.split("\n")

  val word_mat_dir = "/Users/Davidius/BIDMat/"
  val word_mat_name = "words.mat"

  var term_index = mutable.Map.empty[String, Int]

  val num_documents = 2000

  val priors = Array(0.5, 0.5)
  val test_set_size = 100

  val n_folds = 10

  /* Useful. */
  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  /* Writes a dictionary of term->index to a file. */
  def create_dict(): mutable.Map[String,Int]  = {
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

  /* Writes a dictionary of term->index to a file. Was trying to attain
   * performance improvement. */
  def create_dict_for(): mutable.Map[String,Int]  = {
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
  def process(read_index: Boolean = false): Array[BIDMat.SMat] = {
    // Get term_index
    val doc_mats = new Array[BIDMat.SMat](2)
    term_index = mutable.Map.empty[String, Int]
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
    var words_pos_docs: FMat = zeros(num_words, num_documents / 2)
    var words_neg_docs: FMat = zeros(num_words, num_documents / 2)

    for (i <- 0 until pos_files.length) {
      val s = Source.fromFile(example_dir + "pos/" + pos_files(i))
      s.getLines.foreach( (line) => {
        line.split("[\\s.,();:!?&\"]+").foreach({ (word: String) =>
          words_pos_docs(term_index(word), i) += 1
        })
      })
    }
    for (i <- 0 until neg_files.length) {
      val s = Source.fromFile(example_dir + "neg/" + neg_files(i))
      s.getLines.foreach( (line) => {
        line.split("[\\s.,();:!?&\"]+").foreach({ (word: String) =>
          words_neg_docs(term_index(word), i) += 1
        })
      })
    }

    var sparse_words_pos_docs: SMat = sparse(words_pos_docs)
    var sparse_words_neg_docs: SMat = sparse(words_neg_docs)

    //saveAs(word_mat_dir + word_mat_name, sparse_words_docs, "words_docs")
    // Get this error:
    //   java.lang.NoClassDefFoundError: ncsa/hdf/hdf5lib/HDF5Constants

    doc_mats(0) = sparse_words_pos_docs
    doc_mats(1) = sparse_words_neg_docs
    return doc_mats
  }

  /* Given a file_path, returns a binary vector of words present. */
  def file_to_vect(file_path: String): BIDMat.SMat = {
    var words_vect: FMat = zeros(term_index.size, 1)

    val s = Source.fromFile(file_path)
    var t_i = -1
    s.getLines.foreach( (line) => {
      line.split("[\\s.,();:!?&\"]+").foreach({ (word: String) =>
        t_i = term_index(word)
        if (words_vect(t_i, 1) == 0) {
          words_vect(t_i, 1) += 1
        }
      })
    })

    return sparse(words_vect)
  }

  /* Trains a classifer, ie computing log(P(t|c)) for all t and c. */
  def train(docs_fmat: Array[BIDMat.SMat]): Array[Array[Double]] = { //Takes in Integer frequency matrix; outputs Double log-probability matrix
    /* Takes in |V| x |D| matrix doc_fmat of DOCument feature-Frequency MATrix*/
    val pos_docs_fmat: BIDMat.SMat = docs_fmat(0)  //positive documents
    val neg_docs_fmat: BIDMat.SMat = docs_fmat(1)  //negative documents
    val num_features: Int = pos_docs_fmat.length  //number of rows/features
    val num_docs: Int = pos_docs_fmat(0).length //number of columns/documents

    /* Compile into two |V| x 1 matrices pos/neg_freq of aggregate feature-Frequency MATrix */
    // NOTE: You'll want to use BITMat matrices
    val pos_freq = new Array[Double](num_features)  //positive feature frequency matrix
    val neg_freq = new Array[Double](num_features)  //negative feature frequency matrix
    var pos_sum = 0.0 //total word count in positive documents
    var neg_sum = 0.0 //total word count in negative documents
    for (i <- 0 until num_features) { //iterate through rows/features
      var pos_feature = 0.0 //word count for a particular word in positive documents
      var neg_feature = 0.0 //word count for a particular word in negative documents
      for (j <- 0 until num_docs) { //iterate through columns/documents
        pos_feature += pos_docs_fmat(i, j).toDouble //aggregate the same feature across all positive documents
        neg_feature += neg_docs_fmat(i, j).toDouble //aggregate the same feature across all negative documents
      }
      pos_freq(i) = pos_feature //populate i_th row/feature in positive document frequency matrix
      pos_sum += pos_feature //augment total word count in positive documents
      neg_freq(i) = neg_feature //populate i_th row/feature in negative document frequency matrix
      neg_sum += neg_feature //augment total word count in negative documents
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

  /* Returns the beta-weighted F-measure. */
  def f_measure(precision: Double, recall: Double, beta: Int = 1):Double = {
    return ((beta * beta + 1) * precision * recall) / (beta * beta *
             precision * recall)
  }

  /* Performs 10-fold cross-validation, and applies an accuracy measure. */
  def validate(docs: String, folds: Int = n_folds): Double = {
    //PSEUDO-CODE
    var test_set = ""
    var training_set = ""
    var label = 0 //1 or -1
    var result = 0

    var cur_precision = 0.0
    var cur_recall = 0.0

    var f_measure_sum = 0.0

    /* Loop 10 times; segregate training set from test set. */
    for (i <- 0 until 10) {
      var true_positives = 0
      var false_positives = 0
      var false_negatives = 0

      test_set = "" //docs(i) TODO
      training_set = "" //docs - doc(i)

      /* Process into appropriate matrices & compute model. */
      val test_mat = Array(1, 2, 3) // TODO
      val training_mat = Array(1, 2, 3)
      val model = train(training_mat)

      /* Classify & sort */
      for (i <- 0 until test_set_size) {
        result = classify(model, priors, test_mat(i))
        if (result == label && label == 1)
          true_positives += 1
        else if (result != label && label == 1)
          false_positives += 1
        else if (result != label && label == -1)
          false_negatives += 1
      }

      cur_precision = true_positives / (true_positives + false_positives)
      cur_recall = true_positives / (true_positives + false_negatives)

      f_measure_sum += f_measure(cur_precision, cur_recall, 1)
    }

    val f_measure_average = f_measure_sum / folds
    return f_measure_average
  }

  def main(args: Array[String]) = {
    val mat = process(true)
    println(mat(0)(term_index("marilyn"),0))
    println(mat(0)(term_index("campbell"),0))
  }

}

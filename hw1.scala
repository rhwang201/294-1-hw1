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
import stemmer.Stemmer

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

  val stop_words = List("the","is","which","at","on","a","but","we","have",
                        "had","about","for","it","who","to","with","as")

  val word_mat_dir = "/Users/Davidius/BIDMat/"
  val word_mat_name = "words.mat"

  var term_index = mutable.Map.empty[String, Int]

  var stemmer = new Stemmer() // TODO

  val num_documents = 2000
  val alpha: Double = 1.0
  val priors = Array(0.5, 0.5)
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
            if (!stop_words.contains(word) &&
                !term_index.keySet.exists(_ == word)) {
              //stemmer.add(word)
              //stemmer.step1()
              //stemmer.step2()
              //stemmer.step3()
              //stemmer.step4()
              //stemmer.step5a()
              //stemmer.step5b()
              //term_index(stemmer.b) = i
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
            if (!stop_words.contains(word) &&
                !term_index.keySet.exists(_ == word)) {
              //stemmer.add(word)
              //stemmer.step1()
              //stemmer.step2()
              //stemmer.step3()
              //stemmer.step4()
              //stemmer.step5a()
              //stemmer.step5b()
              //term_index(stemmer.b) = i
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
          //stemmer.add(word)
          //stemmer.step1()
          //stemmer.step2()
          //stemmer.step3()
          //stemmer.step4()
          //stemmer.step5a()
          //stemmer.step5b()
          //word = stemmer.b
          if (!term_index.keySet.exists(_ == word)) {
            term_index(word) = z
            z += 1
          }
        }
      })
    }
    println("Finished indexing positive examples")

    files = "ls %sneg".format(example_dir).!!.split("\n")
    for (i <- 0 until files.length) {
      val s = Source.fromFile(example_dir + "neg/" + files(i))
      s.getLines.foreach( (line: String) => {
        var words = line.split("[\\s.,();:!?&\"]+")
        for (k <- 0 until words.length) {
          var word = words(k)
          stemmer.add(word)
          stemmer.step1()
          stemmer.step2()
          stemmer.step3()
          stemmer.step4()
          stemmer.step5a()
          stemmer.step5b()
          word = stemmer.b
          if (!term_index.keySet.exists(_ == word)) {
            term_index(word) = z
            z += 1
          }
        }
      })
    }
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
          //stemmer.add(word)
          //stemmer.step1()
          //stemmer.step2()
          //stemmer.step3()
          //stemmer.step4()
          //stemmer.step5a()
          //stemmer.step5b()
          //var stemmed_word = stemmer.b
          if (!stop_words.contains(word))
            words_pos_docs(term_index(word), i) += 1
        })
      })
    }
    println("Finished processing positive files.")
    for (i <- 0 until neg_files.length) {
      val s = Source.fromFile(example_dir + "neg/" + neg_files(i))
      s.getLines.foreach( (line) => {
        line.split("[\\s.,();:!?&\"]+").foreach({ (word: String) =>
          //stemmer.add(word)
          //stemmer.step1()
          //stemmer.step2()
          //stemmer.step3()
          //stemmer.step4()
          //stemmer.step5a()
          //stemmer.step5b()
          //var stemmed_word = stemmer.b
          if (!stop_words.contains(word))
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
        //stemmer.add(word)
        //stemmer.step1()
        //stemmer.step2()
        //stemmer.step3()
        //stemmer.step4()
        //stemmer.step5a()
        //stemmer.step5b()
        t_i = term_index(word)
        if (words_vect(t_i, 0) == 0) {
          words_vect(t_i, 0) += 1
        }
      })
    })

    return sparse(words_vect)
  }

  /* Trains a classifer, ie computing log(P(t|c)) for all t and c. */
  def train(docs_fmat: Array[BIDMat.SMat]): Array[BIDMat.FMat] = {
    /* Takes in |V| x |D| matrix doc_fmat of DOCument feature-Frequency MATrix*/
    val pos_docs_fmat: BIDMat.SMat = docs_fmat(0)  //positive documents
    val neg_docs_fmat: BIDMat.SMat = docs_fmat(1)  //negative documents
    val num_features: Int = pos_docs_fmat.nrows  //number of rows/features
    val num_docs: Int = pos_docs_fmat.ncols //number of columns/documents

    /* Compile into two |V| x 1 matrices pos/neg_freq of aggregate feature-Frequency MATrix */
    val pos_freq: BIDMat.FMat = sum(pos_docs_fmat.t).t  //positive feature frequency matrix
    val pos_word_count: Double = sum(pos_freq)(0)
    val neg_freq: BIDMat.FMat = sum(neg_docs_fmat.t).t  //negative feature frequency matrix
    val neg_word_count: Double = sum(neg_freq)(0)
    /* Construct two probability matrices for log(P(t|c)) counterpart of frequency entries */
    var pos_prob: BIDMat.FMat = zeros(num_features, 1)  //positive feature log-probability matrix
    var neg_prob: BIDMat.FMat = zeros(num_features, 1)  //negative feature log-probability matrix
    println("Begin building model...")
    for (i <- 0 until num_features) { //iterate through rows/features
      pos_prob(i) = math.log( (pos_freq(i)+alpha) / (pos_word_count+alpha*num_features) ) //compute & populate i_th row/feature in positive log-probability matrix
      neg_prob(i) = math.log( (neg_freq(i)+alpha) / (neg_word_count+alpha*num_features) ) //compute & populate i_th row/feature in negative log-probability matrix
    }
    val model = Array(pos_prob, neg_prob)  //combine positive & negative log-probability matrix into one Probability MATrix
    println("Model completed!  train() successful!")
    return model
  }

  /* Classifies the sentiment level of a given document. */
  def classify(model: Array[BIDMat.FMat], priors: Array[Double], doc: BIDMat.SMat): Int = {    
    /* Compute Maximum A-Posteriori (MAP) estimate for positive/negative sentiment */
    val pos_model: BIDMat.FMat = model(0)
    var pos_map: Double = math.log(priors(0))
    val neg_model: BIDMat.FMat = model(1)
    var neg_map: Double = math.log(priors(1))
    val num_features = pos_model.nrows
    val doc_freq_mat: BIDMat.FMat = full(doc)  //converts an SMat into an FMat
    pos_map += sum(doc_freq_mat *@ pos_model)(0)
    neg_map += sum(doc_freq_mat *@ neg_model)(0)
    /* Output a sentiment level. */
    var sentiment = 0  //a distinctly non-appropriate value
    if (pos_map >= neg_map) {
      sentiment = 1
    } else {
      sentiment = -1
    }
    return sentiment
  }

  /* Returns the beta-weighted F-measure. */
  def f_measure(precision: Double, recall: Double, beta: Int = 1): Double = {
    return ((beta * beta + 1) * precision * recall) / (beta * beta *
             precision + recall)
  }

  /* Performs 10-fold cross-validation, and applies an accuracy measure. */
  def validate(docs: Array[BIDMat.SMat], folds: Int, model: Array[BIDMat.FMat]): Double = {
    val pos_docs: BIDMat.SMat = docs(0)
    val neg_docs: BIDMat.SMat = docs(1)
    val num_docs: Int = pos_docs.nc
    val test_set_size: Int = num_docs / folds
    val num_features: Int = pos_docs.nrows
    var pos_result: Int = 0
    var neg_result: Int = 0
    var cur_precision = 0.0
    var cur_recall = 0.0
    var f_measure_sum = 0.0
    /* Loop 10 times; segregate training set from test set. */
    for (i <- 0 until folds) {
      println("Commencing fold number %d".format(i))
      var true_positives: Double = 0.0
      var false_positives: Double = 0.0
      var false_negatives: Double = 0.0
      /* Process into appropriate matrices & compute model. */
      val pos_test_set: BIDMat.SMat = pos_docs(0 to num_features-1, i*test_set_size to (i+1)*test_set_size-1)
      val pos_training_set: BIDMat.SMat = pos_docs(0 to num_features-1, 0 to i*test_set_size-1) \ pos_docs(0 to num_features-1, (i+1)*test_set_size to num_docs-1) 
      val neg_test_set: BIDMat.SMat = neg_docs(0 to num_features-1, i*test_set_size to (i+1)*test_set_size-1)
      val neg_training_set: BIDMat.SMat = neg_docs(0 to num_features-1, 0 to i*test_set_size-1) \ neg_docs(0 to num_features-1, (i+1)*test_set_size to num_docs-1) 
      val training_set: Array[BIDMat.SMat] = Array(pos_training_set, neg_training_set)
      val model: Array[BIDMat.FMat] = train(training_set)
      /* Classify & sort */
      for (i <- 0 until test_set_size) {
        pos_result = classify(model, priors, pos_test_set(?, i))
        //println("pos_result = %d".format(pos_result))
        neg_result = classify(model, priors, neg_test_set(?, i))
        //println("neg_result = %d".format(neg_result))        
        if (pos_result == 1)
          true_positives += 1
        else
          false_negatives += 1

        if (neg_result == -1)
          true_positives += 1
        else
          false_positives += 1
      }
      println("true_positives = %f; false_negatives = %f; false_positives = %f".format(true_positives, false_negatives, false_positives))
      cur_precision = true_positives / (true_positives + false_positives)
      cur_recall = true_positives / (true_positives + false_negatives)
      f_measure_sum += f_measure(cur_precision, cur_recall, 1)
      println("cur_precision = %f; cur_recall = %f".format(cur_precision, cur_recall))
    }

    val f_measure_average = f_measure_sum / folds
    return f_measure_average
  }

  def main(args: Array[String]) = {
    //create_dict()
    val mat = process(true)
    val model = train(mat)
    //val pos_result = classify( model, priors, file_to_vect("/Users/Davidius/294-1-hw1/review_polarity/txt_sentoken/pos/cv000_29590.txt") )
    //println("Positive result? %d".format(pos_result))
    //val neg_result = classify( model, priors, file_to_vect("/Users/Davidius/294-1-hw1/review_polarity/txt_sentoken/neg/cv000_29416.txt") )
    //println("Negative result? %d".format(neg_result) )
    val outcome = validate(mat, n_folds, model)
    println("f_measure = %f".format(outcome))
    //println(mat(0)(term_index("marilyn"),0))
    //println(mat(0)(term_index("campbell"),0))
  }

}

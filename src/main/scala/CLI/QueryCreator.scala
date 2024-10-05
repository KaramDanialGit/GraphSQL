package CLI

import util.control.Breaks._
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

val alphanumericChars = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).toSet

object QueryCreator {
  def generic_graphql_to_sql(byte_query: Array[Byte]): String = {
    // Note: byte of val 123 = "{" | val of 125 = "}" | byte 10 == EOL

    val string_builder = ArrayBuffer[String]()
    var tmp: String = ""
    var resultMap = Map[String,List[String]]()

    for (byte <- byte_query) {
      if (byte == 10) {
        string_builder += tmp.trim
        tmp = ""
      } else {
        tmp += byte.toChar
      }
    }

    var index = -1
    var last_key: String = ""
    val graph_entries: ListBuffer[String] = ListBuffer()
    var params_map: ListBuffer[(String,String)] = ListBuffer()

    for (line <- string_builder) {
      val line_elements = line.split(" ")

      breakable {
        if (line_elements.length == 2 && line_elements.last == "{") {
          last_key = line_elements(0)
          graph_entries += last_key
        } else if (line_elements.last == "}") {
          break
        } else if (last_key.nonEmpty) {
          params_map = params_map :+ ((last_key, line))
        }
      }
    }

    for (entry <- graph_entries) {
      var tempList = List[String]()
      for (params <- params_map) {
        if (params(0) == entry) {
          tempList = tempList :+ params(1)
        }
      }
      resultMap += (entry -> tempList)
    }

    // TODO: Query parsed to map in the form of graph/table: attribute. Time to translate in create SQL format.
    // NOTE: Dictionary is unordered so use graph_entries as order for joining tables in SQL query
    " "
  }

  def write_to_file(byte_query: String): Unit = {

  }
}

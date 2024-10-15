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

    val final_string: String = map_to_string_query(graph_entries, resultMap, "", 0)
    println(final_string)
    final_string
  }

  def map_to_string_query(table_names: ListBuffer[String], relations: Map[String, List[String]], current_query: String, index: Int): String = {
    if (index >= table_names.length) {
      return current_query
    }

    var tmp: String = current_query

    val first_table: String = table_names(index)

    if (index + 1 == table_names.length) {
      // build single table query
      tmp += "SELECT\n"
      for (param <- relations(first_table)) {
        tmp += f"\t\"$first_table\".\"$param\" AS \"$param\"\n"
      }
      tmp += f"FROM $first_table"
      println(tmp)
    } else {
      // build table pair
      val second_table: String = table_names(index + 1)

      tmp += "SELECT\n"

      for (param <- relations(first_table)) {
        tmp += f"\t\"$first_table\".\"$param\" AS \"$param\"\n"
      }

      for (param <- relations(second_table)) {
        tmp += f"\t\"$second_table\".\"$param\" AS \"$param\"\n"
      }

      tmp += f"FROM $first_table\n"
      tmp += f"LEFT JOIN $second_table ON $first_table.id = $second_table.id\n"

      println(tmp)
      map_to_string_query(table_names, relations, current_query, index + 2)
      println(current_query)
    }
    current_query
  }

  def write_to_file(byte_query: String): Unit = {

  }
}

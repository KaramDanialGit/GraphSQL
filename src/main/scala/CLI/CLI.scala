package CLI

import FileReader.read_file
import QueryCreator.generic_graphql_to_sql, QueryCreator.write_to_file

object CLI {
  def main(args: Array[String]): Unit = {
    intro()
    var user_input: String = ""

    while (user_input != "exit") {
      user_input = scala.io.StdIn.readLine()
      val result = parse_input(user_input)

      val command = result(0)
      val dir = result(1)
      val dest = result(2)

      if (command.isEmpty || dir.isEmpty || dest.isEmpty) {
        println("Please enter your command in a recognizable format")
      } else {
        val graphql_bytes = read_file(dir)
        val sql_query = generic_graphql_to_sql(graphql_bytes)
        write_to_file(sql_query)
      }
    }
  }

  private def intro(): Unit = {
    println("%======================================== Welcome to GraphSQL ========================================%")
    println("To parse your query, pass the command in the following format:")
    println("graph-to-sql -dir <directory of file to parse> -dest <destination of new file>")
    println("%=====================================================================================================%")
  }

  private  def parse_input(input: String): Array[String] = {
    val arguments: Array[String] = input.split(" ")
    var command, directive, destination: String = ""
    val result: Array[String] = new Array(3)

    if (arguments(0) == "graph-to-sql") {
      command = arguments(0)
    } else {
      println("Command not supported!")
    }

    for (x <- 1 until (arguments.length - 1)) {
      if (arguments(x) == "-dir") {
        if (x + 1 < arguments.length && arguments(x + 1)(0) != '-') {
          directive = arguments(x + 1)
        }
      } else if (arguments(x) == "-dest") {
        if (x + 1 < arguments.length && arguments(x + 1)(0) != '-') {
          destination = arguments(x + 1)
        }
      }
    }

    result(0) = command
    result(1) = directive
    result(2) = destination
    result
  }
}

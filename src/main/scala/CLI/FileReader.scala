package CLI

import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.nio.file.Paths

object FileReader {
  def read_file(dir: String): Array[Byte] = {
    val path: Path = Paths.get(dir)
    val buffer = java.nio.file.Files.readAllBytes(path)
    buffer
  }
}

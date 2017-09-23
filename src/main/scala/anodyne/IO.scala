package metriview

import java.io._
import java.nio.charset._
import java.nio.file._
import Paths._
import Files._
import scala.language.implicitConversions
import java.util.stream.{Stream => JStream}
import java.util.stream.Collectors.{toList => toJList}
import java.util.{List => JList}
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._
import java.lang.{ProcessBuilder, Process}
import ProcessBuilder.Redirect

object io {

  def jstreamAsList[T](s: JStream[T]): List[T] = {
    val b = new ListBuffer[T]
    s.forEach(t => b += t)
    b.toList
  }

  def inputStreamAsJStream(s: InputStream): JStream[String] = 
    (new BufferedReader( new InputStreamReader(s, StandardCharsets.UTF_8))).lines

  def writeJStream(p: Path, s: JStream[String]): Unit =
    write(p, s.collect(toJList[String]))

  val cwd = get("")

  def glob(p: String): PathMatcher =
    cwd.getFileSystem.getPathMatcher("glob:" + p)

  def matching(d: Path, g: PathMatcher): List[Path] = {
    for { f <- jstreamAsList(list(d)) if g matches f.getFileName }
    yield f
  }

  def execToFile(args: String*)(out: Path): Int = {
    println("exec " + args.mkString(" "))
    val pb = new ProcessBuilder(args.asJava)
    pb.redirectOutput(out.toFile).redirectError(Redirect.INHERIT)
    pb.start.waitFor
  }

  def execToLines(args: String*): (Int, List[String]) = {
    println("exec " + args.mkString(" "))
    val pb = new ProcessBuilder(args.asJava)
    pb.redirectError(Redirect.INHERIT)
    val p = pb.start
    val ll = jstreamAsList(inputStreamAsJStream(p.getInputStream))
    (p.waitFor, ll)
  }

  def pipeToLines(args: String*)(in: Path): (Int, List[String]) = {
    println("exec " + args.mkString(" "))
    val pb = new ProcessBuilder(args.asJava)
    pb.redirectError(Redirect.INHERIT).redirectInput(in.toFile)
    val p = pb.start
    val ll = jstreamAsList(inputStreamAsJStream(p.getInputStream))
    (p.waitFor, ll)
  }

  def writeLines(p: Path, l: List[String]): Unit =
    write(p, l.asJava)

  def readLines(p: Path): List[String] =
    jstreamAsList(lines(p))


}
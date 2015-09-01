package anodyne

import Matching._

case class CommandLine(args: List[String], flags: Map[String, String]) {
  def shift(n: Int) = CommandLine(args.drop(n), flags)
  def shift: CommandLine = shift(1)
}

object CommandLine {

  val FlagPattern = "--([^=]+)".r
  val FlagValuePattern = "--([^=]+)=(.*)".r

  def Flag(name: String) = extractor[CommandLine, String] { cl => cl.flags get name }
  val Args = seqExtractor[CommandLine, String] { cl => Option(cl.args) }

  def apply(argv: Array[String]): CommandLine =  parse(argv)

  def parse(argv: Array[String]): CommandLine = {
    import scala.collection.mutable.{ListBuffer, HashMap}
    val args = new ListBuffer[String]
    val flags = new HashMap[String, String]

    for( arg <- argv ) arg match {
      case FlagValuePattern(name, value) => flags(name) = value
      case FlagPattern(name) => flags(name) = ""
      case arg => args += arg
    }

    apply(args.toList, flags.toMap)
  }
}

package pamflet

import com.tristanhunt.knockoff._
import scala.util.parsing.input.{ CharSequenceReader, Position, Reader }
import java.io.File

/* Lazy cut-n-paste of fenced.scala. Good enough for now. */
trait DotDiscounter extends Discounter {
  override def newChunkParser : ChunkParser =
    new ChunkParser with DotChunkParser
  override def blockToXHTML: Block => xml.Node = block => block match {
    case DotCodeBlock(text, _, language) =>
      dotChunkToXHTML(text, language)
    case _ => super.blockToXHTML(block)
  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  def dotChunkToXHTML(text: Text, language: Option[String]) = {
    /* generate dot file with contents; invoke dot; provide reference to png */

    val tmp = File.createTempFile("pf-", ".dot")
    printToFile(tmp) { _.println(text.content) }
    val rt = Runtime.getRuntime().exec("dot -o%s.svg %s -Tsvg".format(tmp.getAbsolutePath, tmp.getAbsolutePath))
    rt.waitFor()

    // Read the SVG from the file and put it in the output.
    //<pre>{text.content}</pre>

    // Inexplicably, this takes 5 s on my laptop.
    //xml.XML.loadFile("%s.svg".format(tmp.getAbsolutePath))

    // This is instantaneous, albeit somewhat less safe.
    val rv = scala.io.Source.fromFile("%s.svg".format(tmp.getAbsolutePath)).getLines.reduceLeft{_+_}
    <div>{xml.Unparsed(rv)}</div>
  }
}

trait DotChunkParser extends ChunkParser {
  override def chunk : Parser[ Chunk ] = {
    horizontalRule | leadingStrongTextBlock | leadingEmTextBlock | 
    bulletItem | numberedItem | indentedChunk | header | blockquote | 
    linkDefinition | dotChunk | textBlockWithBreak | textBlock | 
    emptyLines
  }

  def dotChunk : Parser[ Chunk ] =
    dot ~> opt(brush) ~ emptyLine ~
      rep1(unquotedTextLine | emptyLine) <~ endDot <~ emptyLine ^^ {
        case (brush ~ _) ~ lines =>
          DotChunk(foldedString(lines), brush.map { _.content })
      }

  def brush : Parser[Chunk] =
    """[ ]*[^\n]+""".r ^^ { b => TextChunk(b.trim) }

  def dot : Parser[Chunk] =
    "<dot>" ^^ { _ => EmptySpace("") }

  def endDot : Parser[Chunk] =
    "</dot>" ^^ { _ => EmptySpace("") }


  def unquotedTextLine : Parser[ Chunk ] =
    """(?!</dot>)[^\n]+\n""".r ^^ { TextChunk(_) }

  private def foldedString( texts : List[ Chunk ] ) : String =
    ( "" /: texts )( (current, text) => current + text.content )
}

case class DotChunk(val content: String, language: Option[String])
extends Chunk {
  def appendNewBlock( list : collection.mutable.ListBuffer[Block],
                      remaining : List[ (Chunk, Seq[Span], Position) ],
                      spans : Seq[Span], position : Position,
                      discounter : Discounter ) {
    list += DotCodeBlock(Text(content), position, language)
  }
}

case class DotCodeBlock(text: Text, position: Position, 
                           language: Option[String]) extends Block

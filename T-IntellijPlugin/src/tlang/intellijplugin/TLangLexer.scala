package tlang.intellijplugin
import com.intellij.lexer.LexerPosition
import com.intellij.psi.tree.IElementType
import tlang.Context
import tlang.compiler.error.VoidReporter
import tlang.compiler.lexer.{Lexer, Token}
import tlang.utils.StringSource

class TLangLexer extends com.intellij.lexer.Lexer {

  private var buffer: CharSequence       = _
  private var startOffset  = 0
  private var endOffset    = 0
  private var initialState = 0

  private val currentType: IElementType = null
  private val tokenStart  = 0
  private val tokenEnd    = 0

  private var lexer: Lexer       = _
  private var tokens: Array[Token] = _
  private var tokenIndex  = 0

  override def start(buffer: CharSequence, startOffset: Int, endOffset: Int, initialState: Int): Unit = {
    this.buffer = buffer
    this.startOffset = startOffset
    this.endOffset = endOffset
    this.initialState = initialState

    val context = Context(VoidReporter())
    val source = StringSource(buffer.toString, "Lexing")
    lexer = new Lexer(context, source)
    tokens = lexer().toArray
  }

  override def getTokenType: IElementType = new TLangTokenType(tokens(tokenIndex).kind.getClass.getSimpleName)
  override def restore(position: LexerPosition): Unit = {}

  override def getCurrentPosition: LexerPosition = new LexerPosition {
    override def getOffset: Int = 0
    override def getState: Int = 0
  }
  override def advance(): Unit = tokenIndex += 1
  override def getBufferEnd: Int = endOffset
  override def getBufferSequence: CharSequence = buffer
  override def getState: Int = 0
  override def getTokenEnd: Int = tokenEnd
  override def getTokenStart: Int = tokenStart
}

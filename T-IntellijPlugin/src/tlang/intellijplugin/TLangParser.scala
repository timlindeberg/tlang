package tlang.intellijplugin

import com.intellij.lang.{ASTNode, PsiBuilder, PsiParser}
import com.intellij.psi.tree.IElementType

class TLangParser extends PsiParser {
  override def parse(root: IElementType, builder: PsiBuilder): ASTNode = {
    null
  }
}

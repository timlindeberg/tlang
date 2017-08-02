package tlang.intellijplugin;

import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NotNull;

public class TLangTokenType extends IElementType {
    public TLangTokenType(@NotNull String debugName) {
        super(debugName, TLangLanguage.INSTANCE);
    }
    @Override
    public String toString() {
        return "TLangTokenType." + super.toString();
    }

}

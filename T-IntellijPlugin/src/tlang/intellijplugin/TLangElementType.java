package tlang.intellijplugin;

import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

public class TLangElementType extends IElementType {
    public TLangElementType(@NotNull String debugName) {
        super(debugName, TLangLanguage.INSTANCE);
    }
}

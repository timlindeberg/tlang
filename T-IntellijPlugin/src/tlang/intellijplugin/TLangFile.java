package tlang.intellijplugin;

import javax.swing.*;
import com.intellij.extapi.psi.PsiFileBase;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.psi.FileViewProvider;
import org.jetbrains.annotations.NotNull;

public class TLangFile extends PsiFileBase {

    public TLangFile(@NotNull FileViewProvider viewProvider) {
        super(viewProvider, TLangLanguage.INSTANCE);
    }

    @NotNull
    @Override
    public FileType getFileType() {
        return TLangFileType.INSTANCE;
    }

    @Override
    public String toString() {
        return "TLang File";
    }

    @Override
    public Icon getIcon(int flags) {
        return super.getIcon(flags);
    }
}

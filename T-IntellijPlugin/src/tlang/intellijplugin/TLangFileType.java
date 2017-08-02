package tlang.intellijplugin;

import com.intellij.openapi.fileTypes.LanguageFileType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class TLangFileType extends LanguageFileType{

    public static final TLangFileType INSTANCE = new TLangFileType();

    private TLangFileType() {
        super(TLangLanguage.INSTANCE);
    }

    @NotNull
    @Override
    public String getName() {
        return "Tlang file";
    }

    @NotNull
    @Override
    public String getDescription() {
        return "A code file in the T language.";
    }

    @NotNull
    @Override
    public String getDefaultExtension() {
        return "t";
    }

    @Nullable
    @Override
    public Icon getIcon() {
        return TIcon.FILE;
    }
}

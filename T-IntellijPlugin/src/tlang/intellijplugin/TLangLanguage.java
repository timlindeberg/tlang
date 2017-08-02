package tlang.intellijplugin;


import com.intellij.lang.Language;

public class TLangLanguage extends Language {
    public static final TLangLanguage INSTANCE = new TLangLanguage();

    private TLangLanguage() {
        super("Tlang");
    }
}

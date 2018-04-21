import { registerLanguage } from 'react-syntax-highlighter/dist/light';
import {
  ESCAPE_CHAR, KEYWORD, LITERAL, LITERALS, BINARY_NUMBER, HEX_NUMBER, NUMBER, TYPE, VARIABLE, SYMBOL, KEY_WORDS, CHAR,
  TAB,
} from 'syntaxHighlighting/syntax';

registerLanguage('tlang', (hljs: any) => {
  const escapeChar = { className: ESCAPE_CHAR.className, begin: ESCAPE_CHAR.regex };
  return {
    case_insensitive: false,
    keywords: { literal: LITERALS.join(' '), keyword: KEY_WORDS.join(' ') },
    contains: [
      {
        className: 'string',
        variants: [
          {
            begin: '"', end: '"',
            illegal: '\\n',
            contains: [escapeChar],
          },
          {
            begin: '`', end: '`',
            relevance: 10,
          },
        ],
      },
      hljs.C_BLOCK_COMMENT_MODE,
      hljs.C_LINE_COMMENT_MODE,
      { className: CHAR.className, begin: '\'', end: '\'', contains: [escapeChar] },
      ...[
        TAB, KEYWORD, LITERAL, BINARY_NUMBER, HEX_NUMBER, NUMBER, TYPE, VARIABLE, SYMBOL
      ].map(({ regex, className }) => ({ className, begin: regex })),
    ],
  };
});

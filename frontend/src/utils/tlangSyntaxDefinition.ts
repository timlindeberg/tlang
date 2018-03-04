
import { registerLanguage } from 'react-syntax-highlighter/dist/light';

const toRegex = (terms: string[]) => '\\b(' + terms.sort((a, b) => b.length - a.length).join('|') + ')\\b';

const KEY_WORDS = [
  'package',
  'import',
  'is',
  'as',
  'object',
  'class',
  'extension',
  'trait',
  'Def',
  'def',
  'protected',
  'Var',
  'Val',
  'var',
  'val',
  'static',
  'while',
  'for',
  'if',
  'else',
  'return',
  'this',
  'super',
  'new',
  'implicit',
  'print',
  'println',
  'error',
  'break',
  'continue',
  'in',
];

const LITERALS = ['true', 'false', 'null'];

const ESCAPE_CHAR = {
  className: 'escapeChar',
  begin: /\\([tbnrf'"]|(u[0-9a-fA-F]{1,5}))/,
};

const STRING = {
  className: 'string',
  variants: [
    {
      begin: '"', end: '"',
      illegal: '\\n',
      contains: [ESCAPE_CHAR]
    },
    {
      begin: '\'', end: '\'',
      contains: [ESCAPE_CHAR]
    },
    {
      begin: '`', end: '`',
      relevance: 10
    },
  ]
};

const SYMBOL = {
  className: 'symbol',
  begin: /[\-+;.:,=*!#()\[\]{}?~&|%<>/^]/
};

const KEY_WORD = {
  className: 'keyword',
  begin: toRegex(KEY_WORDS),
  relevance: 0
};

const LITERAL = {
  className: 'literal',
  begin: toRegex(LITERALS),
};

const TYPE = {
  className: 'type',
  begin: /\b[A-Z][A-Za-z0-9_]*/,
  relevance: 0
};

const NUMBER = {
  className: 'number',
  begin: /(0b[01_]+[lL]?)|(0x[0-9a-fA-F_]+[lL]?)|((-[0-9]|[0-9])[0-9_]*\.?[0-9_]*([eE]-?[0-9]+)?([fFlL])?)/,
  relevance: 0
};

const CLASS = {
  className: 'class',
  beginKeywords: 'class trait extension',
  end: /[:=\n;]/,
  excludeEnd: true,
  contains: [
    {
      begin: /</,
      end: />/,
      excludeBegin: true,
      excludeEnd: true,
      relevance: 0,
      contains: [TYPE]
    },
  ]
};

const TAB = {
  className: 'tab',
  begin: /➝+/,
};

const METHOD = {
  className: 'function',
  beginKeywords: 'Def def',
  end: /[:=\n;]/,
  excludeEnd: true,
};

const VARIABLE = {
  className: 'variable',
  begin: /[a-zA-Z_][0-9a-zA-Z_]*/,
};

let registered = false;

export default function registerTLang() {
  if (registered) {
    return;
  }

  registered = true;
  registerLanguage('tlang', (hljs: any) => {
    return {
      case_insensitive: false,
      keywords: {
        literal: LITERALS.join(' '),
        keyword: KEY_WORDS.join(' '),
      },
      contains: [
        hljs.C_BLOCK_COMMENT_MODE,
        hljs.C_LINE_COMMENT_MODE,
        STRING,
        NUMBER,
        SYMBOL,
        LITERAL,
        KEY_WORD,
        METHOD,
        CLASS,
        TAB,
        TYPE,
        VARIABLE,
      ]
    };
  });
}

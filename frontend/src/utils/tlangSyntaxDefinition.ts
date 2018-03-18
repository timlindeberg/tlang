import { registerLanguage } from 'react-syntax-highlighter/dist/light';
import * as CodeMirror from 'codemirror';
import 'codemirror/addon/mode/simple';
import 'utils/SyntaxHighlighting.scss';

const joinToRegex = (terms: string[]) => '\\b(' + terms.sort((a, b) => b.length - a.length).join('|') + ')\\b';

const KEY_WORDS = ['package', 'import', 'is', 'as', 'object', 'class', 'extension', 'trait', 'Def', 'def', 'protected',
  'Var', 'Val', 'var', 'val', 'static', 'while', 'for', 'if', 'else', 'return', 'this', 'super', 'new', 'implicit',
  'print', 'println', 'error', 'break', 'continue', 'in'];

const LITERALS = ['true', 'false', 'null'];

const VARIABLE = {
  regex: /[a-zA-Z_][0-9a-zA-Z_]*/,
  className: 'variable'
};
const NUMBER = {
  regex: /(-[0-9]|[0-9])[0-9_]*\.?[0-9_]*([Ee]-?[0-9]+)?([FfLl])?/,
  className: 'number'
};
const BINARY_NUMBER = {
  regex: /0[Bb][01_]+[Ll]?/,
  className: 'number'
};
const HEX_NUMBER = {
  regex: /0[Xx][0-9A-Fa-f_]+[Ll]?/,
  className: 'number'
};
const TYPE = {
  regex: /\b[A-Z][A-Za-z0-9_]*/,
  className: 'type'
};
const LITERAL = {
  regex: joinToRegex(LITERALS),
  className: 'literal'
};
const KEYWORD = {
  regex: joinToRegex(KEY_WORDS),
  className: 'keyword'
};
const SYMBOL = {
  regex: /[\-+;.:,=*!#()\[\]{}?~&|%<>/^]/,
  className: 'symbol'
};
const ESCAPE_CHAR = {
  regex: /\\([\\tbnrf'"]|(u[0-9a-fA-F]{1,5}))/,
  className: 'escapeChar'
};
const LINE_COMMENT = {
  regex: /\/\/.*/,
  className: 'comment'
};

const STRING = { className: 'string' };
const CHAR = { className: 'char' };
const TAB = { className: 'tab' };

(CodeMirror as any).defineSimpleMode('tlang', {
  start: [
    { regex: /"/, token: STRING.className, next: 'string' },
    { regex: /'/, token: 'char', next: 'char' },
    { regex: '\'' + ESCAPE_CHAR.regex.source + '\'', token: ESCAPE_CHAR.className },
    { regex: /`/, token: STRING.className, next: 'multiLineString' },
    { regex: /\/\*/, token: 'comment', next: 'multiLineComment' },
    ...[
      LINE_COMMENT, KEYWORD, LITERAL, BINARY_NUMBER, HEX_NUMBER, NUMBER, TYPE, VARIABLE, SYMBOL
    ].map(({ regex, className }) => ({ regex, token: className })),

    { regex: /=\s*$/, indent: true },
  ],
  char: [
    { regex: ESCAPE_CHAR.regex, token: 'escapeChar', next: 'char' },
    { regex: /.'/, token: 'char', next: 'start' },
    { regex: /'/, token: 'char', next: 'start' },
  ],
  string: [
    { regex: ESCAPE_CHAR.regex, token: 'escapeChar', next: 'string' },
    { regex: /[^\\]+"/, token: STRING.className, next: 'start' },
    { regex: /[^\\]+/, token: STRING.className, next: 'string' },
  ],
  multiLineComment: [
    { regex: /.*?\*\//, token: 'comment', next: 'start' },
    { regex: /.*/, token: 'comment' },
  ],
  multiLineString: [
    { regex: /.*`/, token: 'string', next: 'start' },
    { regex: /.*/, token: 'string' },
  ],
  meta: {
    dontIndentStates: ['multiLineComment', 'multiLineString'],
    lineComment: '//',
  },
});

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
      { className: TAB.className, begin: /\t/ },
      ...[
        KEYWORD, LITERAL, BINARY_NUMBER, HEX_NUMBER, NUMBER, TYPE, VARIABLE, SYMBOL
      ].map(({ regex, className }) => ({ className, begin: regex })),
    ],
  };
});

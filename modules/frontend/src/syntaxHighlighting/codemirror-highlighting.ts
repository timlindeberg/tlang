import * as CodeMirror from 'codemirror';
import 'codemirror/addon/mode/simple';
import {
  BINARY_NUMBER, CHAR, COMMENT, ESCAPE_CHAR, HEX_NUMBER, KEYWORD, LITERAL, NUMBER, STRING, SYMBOL, TYPE, VARIABLE,
} from 'syntaxHighlighting/syntax';

(CodeMirror as any).defineSimpleMode('tlang', {
  start: [
    { regex: /"/, token: STRING.className, next: 'string' },
    { regex: /'/, token: CHAR.className, next: 'char' },
    { regex: /`/, token: STRING.className, next: 'multiLineString' },
    { regex: /\/\*/, token: COMMENT.className, next: 'multiLineComment' },
    ...[
      COMMENT, KEYWORD, LITERAL, BINARY_NUMBER, HEX_NUMBER, NUMBER, TYPE, VARIABLE, SYMBOL
    ].map(({ regex, className }) => ({ regex, token: className })),

    { regex: /=\s*$/, indent: true },
  ],
  char: [
    { regex: ESCAPE_CHAR.regex, token: 'escapeChar', next: 'char' },
    { regex: /.'/, token: CHAR.className, next: 'start' },
    { regex: /'/, token: CHAR.className, next: 'start' },
  ],
  string: [
    { regex: ESCAPE_CHAR.regex, token: 'escapeChar', next: 'string' },
    { regex: /[^\\]*"/, token: STRING.className, next: 'start' },
    { regex: /[^\\]*/, token: STRING.className, next: 'string' },
  ],
  multiLineComment: [
    { regex: /.*?\*\//, token: COMMENT.className, next: 'start' },
    { regex: /.*/, token: COMMENT.className },
  ],
  multiLineString: [
    { regex: /.*`/, token: STRING.className, next: 'start' },
    { regex: /.*/, token: STRING.className },
  ],
  meta: {
    dontIndentStates: ['multiLineComment', 'multiLineString'],
    lineComment: '//',
  },
});

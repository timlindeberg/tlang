import 'syntaxHighlighting/SyntaxHighlighting.scss';

const joinToRegex = (terms: string[]) => '\\b(' + terms.sort((a, b) => b.length - a.length).join('|') + ')\\b';

export const KEY_WORDS = ['package', 'import', 'is', 'as', 'object', 'class', 'extension', 'trait', 'Def', 'def',
  'protected', 'Var', 'Val', 'var', 'val', 'static', 'while', 'for', 'if', 'else', 'return', 'this', 'super', 'new',
  'implicit', 'print', 'println', 'error', 'break', 'continue', 'in'];

export const LITERALS = ['true', 'false', 'null'];

export const VARIABLE = {
  regex: /[a-zA-Z_][0-9a-zA-Z_]*/,
  className: 'variable',
};
export const NUMBER = {
  regex: /(-[0-9]|[0-9])[0-9_]*\.?[0-9_]*([Ee]-?[0-9]+)?([FfLl])?/,
  className: 'number',
};
export const BINARY_NUMBER = {
  regex: /0[Bb][01_]+[Ll]?/,
  className: 'number',
};
export const HEX_NUMBER = {
  regex: /0[Xx][0-9A-Fa-f_]+[Ll]?/,
  className: 'number',
};
export const TYPE = {
  regex: /\b[A-Z][A-Za-z0-9_]*/,
  className: 'type',
};
export const LITERAL = {
  regex: joinToRegex(LITERALS),
  className: 'literal',
};
export const KEYWORD = {
  regex: joinToRegex(KEY_WORDS),
  className: 'keyword',
};
export const SYMBOL = {
  regex: /[\-+;.:,=*!#()\[\]{}?~&|%<>/^]/,
  className: 'symbol',
};
export const ESCAPE_CHAR = {
  regex: /\\([\\tbnrf'"]|(u[0-9a-fA-F]{1,5}))/,
  className: 'escapeChar',
};
export const COMMENT = {
  regex: /\/\/.*/,
  className: 'comment',
};
export const TAB = {
  regex: /\t/,
  className: 'tab',
};

export const STRING = { className: 'string' };
export const CHAR = { className: 'char' };

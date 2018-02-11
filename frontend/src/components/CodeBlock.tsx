import * as React from 'react';
import SyntaxHighlighter from 'react-syntax-highlighter';
import { registerLanguage } from 'react-syntax-highlighter/dist/light';
import 'components/CodeBlock.scss';

interface CodeBlockProps {
  children: string;
}

registerLanguage('tlang', (hljs: any) => {

  const STRING = {
    className: 'string',
    variants: [
      {
        begin: '"', end: '"',
        illegal: '\\n',
        contains: [hljs.BACKSLASH_ESCAPE]
      },
      {
        begin: '\'', end: '\'',
        contains: [hljs.BACKSLASH_ESCAPE]
      },
      {
        begin: '`', end: '`',
        relevance: 10
      },
      {
        begin: '[a-z]+"', end: '"',
        illegal: '\\n',
        contains: [hljs.BACKSLASH_ESCAPE]
      },
    ]
  };

  const SYMBOL = {
    className: 'symbol',
    begin: /[\-+;.:,=*!#()\[\]{}?~&|%<>/^]/
  };

  const TYPE = {
    className: 'type',
    begin: /[A-Z][A-Za-z0-9_]*/,
    relevance: 0
  };

  const NUMBER = {
    className: 'number',
    begin: /(-[0-9]|[0-9])[0-9_]*\.?[0-9_]*([eE]-?[0-9]+)?([fFlL])?/,
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

  const METHOD = {
    className: 'function',
    beginKeywords: 'Def def',
    end: /[:=\n;]/,
    excludeEnd: true,
  };

  return {
    case_insensitive: false,
    keywords: {
      literal: 'true false null',
      keyword: 'Def package import is as object class extension trait def protected Var Val var val static while for ' +
      'if else return this super new implicit print println error break continue in'
    },
    contains: [
      hljs.COMMENT(
        /\/\*/,      // begin regex
        /\*\//,        // end regex
      ),
      hljs.C_LINE_COMMENT_MODE,
      STRING,
      NUMBER,
      SYMBOL,
      TYPE,
      METHOD,
      CLASS,
      hljs.C_NUMBER_MODE,
    ]
  };
});

export default class CodeBlock extends React.Component<CodeBlockProps> {

  render() {
    const { children } = this.props;
    return (
      <SyntaxHighlighter
        wrapLines
        useInlineStyles={false}
        lineProps={{ className: 'hljs-font' }}
        showLineNumbers
        language="tlang"
      >
        {children}
      </SyntaxHighlighter>
    );
  }

}
import * as React from 'react';

import { AST } from 'types/markdown';

import DocumentationSidebar from 'components/documentation/DocumentationSidebar';
import Documentation from 'components/documentation/Documentation';
import unified from 'unified';
import remarkParse from 'remark-parse';

import 'components/documentation/DocumentationView.scss';
import MenuLayout from 'components/layout/MenuLayout';

const findFiles = (ctx: any): string[] => {
  const keys = ctx.keys();
  return keys.map(ctx);
};

const markdownFiles = findFiles(require.context('documentation', true, /\.md$/));

interface DocumentationViewState {
  markdown: AST[];
  active: number;
}

export default class DocumentationView extends React.Component<{}, DocumentationViewState> {

  state: DocumentationViewState = { markdown: [], active: 0 };

  componentDidMount() {
    const markdown = markdownFiles.map(content => unified().use(remarkParse).parse(content));
    this.setState(() => ({ markdown }));
  }

  setActive = (active: number) => {
    if (active !== this.state.active) {
      this.setState(() => ({ active }));
    }
  }

  render() {
    const { markdown, active } = this.state;
    return (
      <MenuLayout
        menu={<DocumentationSidebar markdown={markdown} active={active}/>}
        content={<Documentation markdown={markdown} active={active} setActive={this.setActive} />}
      />
    );
  }
}

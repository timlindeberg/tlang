import * as React from 'react';

import * as API from 'api';
import { AST } from 'types/markdown';

import DocumentationSidebar from 'components/documentation/DocumentationSidebar';
import Documentation from 'components/documentation/Documentation';
import unified from 'unified';
import remarkParse from 'remark-parse';

import 'components/documentation/DocumentationView.scss';
import MenuLayout from 'components/layout/MenuLayout';

interface DocumentationViewState {
  markdown: AST[];
  active: number;
}

export default class DocumentationView extends React.Component<{}, DocumentationViewState> {

  state: DocumentationViewState = { markdown: [], active: 0 };

  async componentDidMount() {
    const markdownContent = await API.getDocumentation();
    const markdown = markdownContent.map(content => unified().use(remarkParse).parse(content));
    this.setState(() => ({ markdown }));
  }

  setActive = (active: number) => {
    if (active !== this.state.active) {
      this.setState(() => ({ active }));
    }
  }

  Menu = () => {
    return <DocumentationSidebar {...this.state}/>;
  }

  Content = () => {
    return <Documentation {...this.state} setActive={this.setActive} />;
  }

  render() {
    return <MenuLayout menu={this.Menu} content={this.Content}/>;
  }
}

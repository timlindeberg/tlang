import * as React from 'react';

import { Grid, Segment, Button, Icon } from 'semantic-ui-react';
import { AST } from 'types/markdown';

import Navbar from 'components/Navbar';
import DocumentationSidebar from 'components/documentation/DocumentationSidebar';
import Documentation from 'components/documentation/Documentation';
import Logo from 'components/Logo';
import unified from 'unified';
import remarkParse from 'remark-parse';

import 'components/documentation/DocumentationView.scss';

const findFiles = (ctx: any): string[] => {
  const keys = ctx.keys();
  return keys.map(ctx);
};

const markdownFiles = findFiles(require.context('documentation', true, /\.md$/));

interface DocumentationViewState {
  markdown: AST[];
  menuVisible: boolean;
  active?: string;
}

export default class DocumentationView extends React.Component<{}, DocumentationViewState> {

  state: DocumentationViewState = { markdown: [], menuVisible: true };

  componentDidMount() {
    const markdown = markdownFiles.map(content => unified().use(remarkParse).parse(content));
    this.setState(() => ({ markdown }));
  }

  setActive = (active: string) => {
    if (active !== this.state.active) {
      this.setState(() => ({ active }));
    }
  }

  toggleVisibility = (): void => this.setState(prev => ({ menuVisible: !prev.menuVisible }));

  render() {
    const { markdown, menuVisible, active } = this.state;

    const rightSide = (
      <React.Fragment>
        <Segment inverted id="Documentation-navbar">
          <Grid>
            <Grid.Column>
              <Button icon size="large" onClick={this.toggleVisibility}>
                <Icon name={menuVisible ? 'angle double left' : 'angle double right'}/>
              </Button>
            </Grid.Column>
            <Grid.Column>
              <Navbar/>
            </Grid.Column>
          </Grid>
        </Segment>
        <Documentation markdown={markdown} setActive={this.setActive} />
      </React.Fragment>
    );

    if (!menuVisible) {
      return rightSide;
    }

    const leftSide = (
      <React.Fragment>
        <Segment inverted id="Documentation-logo">
          <Logo size={2.5}/>
        </Segment>
        <Segment inverted id="Documentation-menu">
          <DocumentationSidebar markdown={markdown} visible={menuVisible} active={active}/>
        </Segment>
      </React.Fragment>
    );

    return (
      <Grid>
        <Grid.Column width={4} id="Documentation-left">{leftSide}</Grid.Column>
        <Grid.Column width={12} id="Documentation-right">{rightSide}</Grid.Column>
      </Grid>
    );
  }
}

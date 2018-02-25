import * as React from 'react';
import { Grid, Segment, Button, Icon, Sidebar } from 'semantic-ui-react';
import Navbar from 'components/Navbar';
import { AST } from 'typescriptDeclarations/markdownParser';
import remarkParse from 'remark-parse';
import unified from 'unified';
import DocumentationSidebar from 'components/documentation/DocumentationSidebar';
import Documentation from 'components/documentation/Documentation';
import 'components/documentation/Documentation.scss';

const findFiles = (ctx: any): string[] => {
  const keys = ctx.keys();
  return keys.map(ctx);
};

const markdownFiles = findFiles(require.context('documentation', true, /\.md$/));

interface DocumentationViewState {
  markdown: AST[];
  visible: boolean;
  active: string;
}

export default class DocumentationView extends React.Component<{}, DocumentationViewState> {

  state: DocumentationViewState = { markdown: [], visible: true, active: '' };

  componentDidMount() {
    const markdown = markdownFiles.map(content => unified().use(remarkParse).parse(content));
    document.documentElement.addEventListener('scroll', event => console.log(event));
    this.setState(() => ({ markdown }));
  }

  setActive = (active: string) => this.setState(() => ({ active }));

  toggleVisibility = (): void => this.setState(prev => ({ visible: !prev.visible }));

  render() {
    const { markdown, visible, active } = this.state;
    return (
      <Sidebar.Pushable>
        <DocumentationSidebar
          markdown={markdown}
          visible={visible}
          active={active}
        />
        <Sidebar.Pusher>
          <Segment inverted style={{ borderRadius: 0, margin: 0 }}>
            <Grid>
              <Grid.Column textAlign="center" verticalAlign="middle">
                <Button icon size="large" onClick={this.toggleVisibility}>
                  <Icon name={visible ? 'angle double left' : 'angle double right'}/>
                </Button>
              </Grid.Column>
              <Grid.Column>
                <Navbar/>
              </Grid.Column>
            </Grid>
          </Segment>
          <Documentation markdown={markdown} setActive={this.setActive}/>
        </Sidebar.Pusher>
      </Sidebar.Pushable>
    );
  }
}

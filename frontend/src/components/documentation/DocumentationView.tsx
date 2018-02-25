import * as React from 'react';
import { Grid, Menu, Segment, Divider, Button, Icon, Sidebar } from 'semantic-ui-react';
import Logo from 'components/Logo';
import Navbar from 'components/Navbar';
import astToReact, { AST, Type } from 'typescriptDeclarations/markdownParser';
import remarkParse from 'remark-parse';
import unified from 'unified';
import { HashLink } from 'react-router-hash-link';

const findFiles = (ctx: any): string[] => {
  const keys = ctx.keys();
  return keys.map(ctx);
};

const markdownFiles = findFiles(require.context('documentation', true, /\.md$/));

interface DesktopContainerState {
  markdown: AST[];
  visible: boolean;
}

interface Header {
  value: string;
  children: Header[];
}

export default class DesktopContainer extends React.Component<{}, DesktopContainerState> {

  state: DesktopContainerState = { markdown: [], visible: true };

  componentDidMount() {
    const markdown = markdownFiles.map(content => unified().use(remarkParse).parse(content));
    this.setState(() => ({ markdown }));
  }

  parseHeaders = (): Header[] => {
    const headers: Header[] = [];
    this.state.markdown.forEach((ast) => {
      const headings = ast.children.filter(child => child.type === Type.Heading && (child.depth!) < 3);
      if (headings.length === 0) {
        return;
      }

      let header: Header | null = null;
      headings.forEach(({ children, depth }) => {
        const newHeader = { children: [], value: children[0].value! };
        if (depth === 1) {
          if (header) {
            headers.push(header);
          }
          header = newHeader;
        } else {
          header!.children.push(newHeader);
        }
      });
      headers.push(header!);
    });
    return headers;
  }

  renderMenu = (): JSX.Element => {
    const headers = this.parseHeaders();
    const MenuLink = ({ header, children }: any) => (
      <Menu.Item as={HashLink} to={`#${header.value.replace(/ /g, '-')}`} onClick={e => e.stopPropagation()}>
        {header.value}
        {children}
      </Menu.Item>
    );

    const menuItems = headers.map((header, i) => {
      return (
        <MenuLink key={i} header={header}>
          {header.children.length > 0 && (
            <Menu.Menu>
              {header.children.map((subHeader, j) => <MenuLink key={j} header={subHeader}/>)}
            </Menu.Menu>
          )}
        </MenuLink>
      );
    });

    return (
      <Sidebar as={Segment} animation="slide along" visible={this.state.visible} inverted>
        <Logo size={3}/>
        <Menu inverted fluid vertical size="small">{menuItems}</Menu>
      </Sidebar>
    );
  }

  renderDocumentation = (): JSX.Element[] => {
    let documentation: JSX.Element[] = [];
    const markdown = this.state.markdown;
    markdown.forEach((ast, i) => {
      documentation = documentation.concat(astToReact(ast));
      if (i < markdown.length - 1) {
        documentation.push(<Divider style={{ marginTop: '2em', marginBottom: '2em' }}/>);
      }
    });
    return documentation;
  }

  toggleVisibility = (): void => this.setState(prev => ({ visible: !prev.visible }));

  render() {
    const visible = this.state.visible;
    const icon = visible ? 'angle double left' : 'angle double right';
    console.log('icon', icon);
    return (
      <Sidebar.Pushable>
        {this.renderMenu()}
        <Sidebar.Pusher style={{ height: '100vh', overflowY: 'scroll' }}>
          <Segment inverted style={{ borderRadius: 0, margin: 0 }}>
            <Grid>
              <Grid.Column textAlign="center" verticalAlign="middle">
                <Button icon secondary size="small" onClick={this.toggleVisibility}>
                  <Icon name={icon}/>
                </Button>
              </Grid.Column>
              <Grid.Column>
                <Navbar/>
              </Grid.Column>
            </Grid>
          </Segment>
          <Segment style={{ borderRadius: 0, margin: 0, padding: '1.5em' }}>
            {this.renderDocumentation()}
          </Segment>
        </Sidebar.Pusher>
      </Sidebar.Pushable>
    );
  }
}

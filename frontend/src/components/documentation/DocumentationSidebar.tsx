import * as React from 'react';
import { Menu, Segment, Sidebar } from 'semantic-ui-react';
import { AST, Type } from 'typescriptDeclarations/markdownParser';
import Logo from 'components/Logo';
import { HashLink } from 'react-router-hash-link';
import { Collapse } from 'react-collapse';

interface Header {
  value: string;
  children: Header[];
  parent?: Header;
}

interface DocumentationSidebarProps {
  markdown: AST[];
  visible: boolean;
  active: string;
}

export default class DocumentationSidebar extends React.Component<DocumentationSidebarProps, {}> {

  parseHeaders = (): Header[] => {
    const headers: Header[] = [];
    this.props.markdown.forEach((ast) => {
      const headings = ast.children.filter(child => child.type === Type.Heading && (child.depth!) < 3);
      if (headings.length === 0) {
        return;
      }

      let header: Header | null = null;
      headings.forEach(({ children, depth }) => {
        const newHeader: Header = { children: [], value: children[0].value! };
        if (depth === 1) {
          if (header) {
            headers.push(header);
          }
          header = newHeader;
        } else {
          newHeader.parent = header!;
          header!.children.push(newHeader);
        }
      });
      headers.push(header!);
    });
    return headers;
  }

  render() {
    const headers = this.parseHeaders();
    const active = this.props.active;

    const isActive = (header: Header): boolean => active === header.value || header.children.some(isActive);

    const menuItems = headers.map((header, i) => {
      return (
        <Menu.Item key={i}>
          <HashLink to={`#${header.value.replace(/ /g, '-')}`} smooth>{header.value}</HashLink>
          {header.children.length > 0 && (
            <Collapse isOpened={isActive(header)}>
              <Menu.Menu>
                {header.children.map(({ value }) => (
                  <Menu.Item
                    key={value}
                    active={active === value}
                    as={HashLink}
                    to={`#${value.replace(/ /g, '-')}`}
                    onClick={e => e.stopPropagation()}
                    style={{ paddingLeft: '2em' }}
                  >
                    <span>{value}</span>
                  </Menu.Item>
                ))}
              </Menu.Menu>
            </Collapse>
          )}
        </Menu.Item>
      );
    });

    return (
      <Sidebar as={Segment} animation="slide along" visible={this.props.visible} inverted>
        <Logo size={3}/>
        <Menu inverted fluid vertical size="small">{menuItems}</Menu>
      </Sidebar>
    );
  }
}

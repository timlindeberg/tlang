import * as React from 'react';

import { Menu } from 'semantic-ui-react';
import { AST, Type } from 'types/markdown';
import { HashLink } from 'react-router-hash-link';
import 'components/documentation/DocumentationSidebar.scss';

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

    const anchor = (value: string) => `#${value.replace(/ /g, '-')}`;
    // inline: 'nearest' fixes an issue of the window moving horizontally when scrolling.
    const scrollBehavior = (el: Element) => el.scrollIntoView({
      behavior: 'smooth',
      block: 'center',
      inline: 'nearest',
    });

    const isActive = (header: Header): boolean => active === header.value || header.children.some(isActive);
    return (
      <Menu inverted borderless fluid vertical size="small" id="DocMenu">
        { headers.map(header => (
          <Menu.Item key={header.value} active={isActive(header)}>
            <HashLink to={anchor(header.value)} scroll={scrollBehavior}>{header.value}</HashLink>
            { header.children.length > 0 && (
              <Menu.Menu>
                {header.children.map(({ value }) => (
                  <Menu.Item
                    active={active === value}
                    as={HashLink}
                    to={anchor(value)}
                    onClick={e => e.stopPropagation()}
                    style={{ paddingLeft: '2em' }}
                    scroll={scrollBehavior}
                  >
                    <span>{value}</span>
                  </Menu.Item>
                ))}
              </Menu.Menu>
            )}
          </Menu.Item>
          ))
        }
      </Menu>
    );
  }
}

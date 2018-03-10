import * as React from 'react';

import * as _ from 'lodash';

import { Accordion, Menu, Search } from 'semantic-ui-react';
import { AST, Type } from 'types/markdown';
import { HashLink } from 'react-router-hash-link';
import 'components/documentation/DocumentationSidebar.scss';
import { Collapse } from 'react-collapse';

interface Header {
  index: number;
  value: string;
  children: Header[];
}

interface DocumentationSidebarProps {
  markdown: AST[];
  visible: boolean;
  active: number;
}

interface DocumentationSidebarState {
  headers: Header[];
  searchValue: string;
  searchResults: string[];
  mousedOverHeaders: Set<Header>;
}

const MIN_CHARS = 3;

export default class DocumentationSidebar
  extends React.Component<DocumentationSidebarProps, DocumentationSidebarState> {

  originalHeaders: Header[] = [];
  headerValues: string[] = [];
  state: DocumentationSidebarState = {
    headers: [],
    searchValue: '',
    searchResults: [],
    mousedOverHeaders: new Set(),
  };

  constructor(props: DocumentationSidebarProps) {
    super(props);
    this.originalHeaders = this.parseHeaders(props.markdown);
  }

  componentDidMount() {
    this.setState(() => ({ headers: this.originalHeaders.slice() }));
  }

  componentWillReceiveProps(nextProps: DocumentationSidebarProps) {
    if (nextProps.markdown !== this.props.markdown) {
      this.originalHeaders = this.parseHeaders(nextProps.markdown);
    }
  }

  parseHeaders = (markdown: AST[]): Header[] => {
    const headers: Header[] = [];
    let headerIndex = 0;
    markdown.forEach((ast) => {
      const headings = ast.children.filter(child => child.type === Type.Heading && (child.depth!) < 3);
      if (headings.length === 0) {
        return;
      }

      let header: Header | null = null;
      headings.forEach(({ children, depth }) => {
        const value = children[0].value!;
        this.headerValues.push(value);
        const newHeader: Header = { value, index: headerIndex++, children: [] };
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

  handleSearchChange = (e: any, { value }: any) => {
    this.setState(() => ({ searchValue: value }));
    if (value.length < MIN_CHARS) {
      this.setState(() => ({ searchResults: [], headers: this.originalHeaders }));
      return;
    }

    const re = new RegExp(_.escapeRegExp(value), 'i');
    const searchResults = this.headerValues.filter(v => re.test(v));

    const included = (header: Header): boolean =>
      searchResults.some(v => v === header.value) || header.children.some(included);

    const filteredHeaders = this.originalHeaders.slice()
      .filter(included)
      .map((header: Header) => {
        const newHeader = Object.assign({}, header);
        newHeader.children = header.children.slice().filter(included);
        return newHeader;
      });

    this.setState({ searchResults, headers: filteredHeaders });
  }

  onSearchKeyDown = (e: any) => {
    const ENTER = 13;
    if (e.keyCode !== ENTER) {
      return;
    }

    const searchResult = this.state.searchResults[0];
    if (!searchResult) {
      return;
    }

    const element = document.getElementById(this.toId(searchResult))!;
    this.scrollTo(element);
  }

  toId = (value: String) => value.replace(/ /g, '-');
  anchor = (value: string) => `#${this.toId(value)}`;

  // inline: 'nearest' fixes an issue of the window moving horizontally when scrolling.
  scrollTo = (el: Element) => el.scrollIntoView({ behavior: 'smooth', block: 'start', inline: 'nearest' });

  mouseEnterHeader = (header: Header) => {
    const mousedOverHeaders = new Set(this.state.mousedOverHeaders);
    mousedOverHeaders.add(header);
    this.setState(() => ({ mousedOverHeaders }));
  }

  mouseLeaveMenu = () => this.setState(() => ({ mousedOverHeaders: new Set() }));

  SearchBar = () => {
    return (
      <Search
        size="mini"
        value={this.state.searchValue}
        onSearchChange={this.handleSearchChange}
        onKeyDown={this.onSearchKeyDown}
        open={false}
      />
    );
  }

  Menu = () => {
    const active = this.props.active;
    const { searchValue, headers, mousedOverHeaders } = this.state;

    const isSearching = searchValue.length >= MIN_CHARS;
    const isActive = (header: Header): boolean => active === header.index || header.children.some(isActive);
    return (
      <Accordion
        as={Menu}
        inverted
        borderless
        fluid
        vertical
        size="small"
        id="DocMenu"
        onMouseLeave={this.mouseLeaveMenu}
      >
        { headers.map((header) => {
          const isHeaderActive = isActive(header);
          const hasBeenMousedOver = mousedOverHeaders.has(header);
          const isHeaderOpen = hasBeenMousedOver || isSearching || isHeaderActive;
          return (
            <Menu.Item key={header.value} active={isHeaderActive} onMouseEnter={() => this.mouseEnterHeader(header)}>
              <Accordion.Title
                as={HashLink}
                to={this.anchor(header.value)}
                scroll={this.scrollTo}
                active={isHeaderOpen}
                content={header.value}
              />
              <Menu.Menu as={Collapse} isOpened={isHeaderOpen}>
                <div>
                  { header.children.map(({ value, index }) => (
                    <Menu.Item
                      key={value}
                      active={active === index}
                      as={HashLink}
                      to={this.anchor(value)}
                      onClick={e => e.stopPropagation()}
                      style={{ paddingLeft: '2em' }}
                      scroll={this.scrollTo}
                    >
                      <span>{value}</span>
                    </Menu.Item>
                  ))}
                </div>
              </Menu.Menu>
            </Menu.Item>
          );
        })}
      </Accordion>
    );
  }

  render() {
    return (
      <div id="Documentation-menu-inner">
        <this.SearchBar/>
        <this.Menu/>
      </div>
    );
  }
}

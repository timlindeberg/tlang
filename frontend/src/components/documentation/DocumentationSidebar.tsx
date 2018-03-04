import * as React from 'react';

import * as _ from 'lodash';

import { Accordion, Divider, Menu, Search } from 'semantic-ui-react';
import { AST, Type } from 'types/markdown';
import { HashLink } from 'react-router-hash-link';
import 'components/documentation/DocumentationSidebar.scss';
import { Collapse } from 'react-collapse';

interface Header {
  value: string;
  children: Header[];
  parent?: Header;
}

interface DocumentationSidebarProps {
  markdown: AST[];
  visible: boolean;
  active?: string;
}

interface DocumentationSidebarState {
  headers: Header[];
  searchValue: string;
  searchResults: string[];
  mouseOver?: Header;
}

const MIN_CHARS = 3;

export default class DocumentationSidebar
  extends React.Component<DocumentationSidebarProps, DocumentationSidebarState> {

  originalHeaders: Header[] = [];
  headerValues: string[] = [];
  state: DocumentationSidebarState = { headers: [], searchValue: '', searchResults: [] };

  constructor(props: DocumentationSidebarProps) {
    super(props);
    this.parseHeaders(props.markdown);
  }

  componentDidMount() {
    this.setState(() => ({ headers: this.originalHeaders.slice() }));
  }

  componentWillReceiveProps(nextProps: DocumentationSidebarProps) {
    if (nextProps.markdown !== this.props.markdown) {
      this.parseHeaders(nextProps.markdown);
    }
  }

  parseHeaders = (markdown: AST[]): void => {
    this.originalHeaders = [];
    markdown.forEach((ast) => {
      const headings = ast.children.filter(child => child.type === Type.Heading && (child.depth!) < 3);
      if (headings.length === 0) {
        return;
      }

      let header: Header | null = null;
      headings.forEach(({ children, depth }) => {
        const value = children[0].value!;
        this.headerValues.push(value);
        const newHeader: Header = { value, children: [] };
        if (depth === 1) {
          if (header) {
            this.originalHeaders.push(header);
          }
          header = newHeader;
        } else {
          newHeader.parent = header!;
          header!.children.push(newHeader);
        }
      });
      this.originalHeaders.push(header!);
    });
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

  toId = (value: String) => value.replace(/ /g, '-');
  anchor = (value: string) => `#${this.toId(value)}`;

  // inline: 'nearest' fixes an issue of the window moving horizontally when scrolling.
  scrollTo = (el: Element) => el.scrollIntoView({ behavior: 'smooth', block: 'start', inline: 'nearest' });

  mouseEnterHeader = (header: Header) => this.setState(() => ({ mouseOver: header }));
  mouseLeaveHeader = () => this.setState(() => ({ mouseOver: undefined }));

  Menu = () => {
    const active = this.props.active;
    const { searchValue, headers, mouseOver } = this.state;

    const isSearching = searchValue.length >= MIN_CHARS;
    const isActive = (header: Header): boolean => active === header.value || header.children.some(isActive);
    return (
      <Accordion as={Menu} inverted borderless fluid vertical size="small" id="DocMenu">
        { headers.map((header, i) => {
          const isHeaderActive = active ? isActive(header) : i === 0;
          const isHeaderOpen = mouseOver === header || isSearching || isHeaderActive;
          return (
            <Menu.Item
              key={header.value}
              active={isHeaderActive}
              onMouseEnter={() => this.mouseEnterHeader(header)}
              onMouseLeave={this.mouseLeaveHeader}
            >
              <Accordion.Title
                as={HashLink}
                to={this.anchor(header.value)}
                scroll={this.scrollTo}
                active={isHeaderOpen}
                content={header.value}
              />
              <Menu.Menu as={Collapse} isOpened={isHeaderOpen}>
                <div>
                  { header.children.map(({ value }) => (
                    <Menu.Item
                      key={value}
                      active={active === value}
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

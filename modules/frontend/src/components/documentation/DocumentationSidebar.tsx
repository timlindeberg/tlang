import * as React from 'react';

import * as _ from 'lodash';

import 'components/documentation/DocumentationSidebar.less';
import { Collapse } from 'react-collapse';
import { HashLink } from 'react-router-hash-link';
import { Accordion, Menu, Search } from 'semantic-ui-react';
import { AST, Markdown } from 'types/markdown';
import { scrollTo } from 'utils/misc';

interface Header {
  index: number;
  value: string;
  children: Header[];
}

interface DocumentationSidebarProps {
  markdown: AST[];
  active: number;
}

interface DocumentationSidebarState {
  headers: Header[];
  searchValue: string;
  searchResults: string[];
  mousedOverHeaders: Set<Header>;
}

const MIN_CHARS = 3;

export default class DocumentationSidebar extends React.Component<DocumentationSidebarProps, DocumentationSidebarState>
{

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
  }

  componentDidMount() {
    this.updateHeaders(this.props);
  }

  componentWillReceiveProps(nextProps: DocumentationSidebarProps) {
    if (nextProps.markdown !== this.props.markdown) {
      this.updateHeaders(nextProps);
    }
  }

  updateHeaders = (props: DocumentationSidebarProps) => {
    this.originalHeaders = this.parseHeaders(props.markdown);
    this.setState(() => ({ headers: this.originalHeaders.slice() }));
  }

  parseHeaders = (markdown: AST[]): Header[] => {
    const headers: Header[] = [];
    let headerIndex = 0;
    markdown.forEach((ast) => {
      const headings = ast.children.filter(child => child.type === Markdown.Heading && (child.depth!) < 3);
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
    scrollTo(element);
  }

  toId = (value: String) => value.replace(/ /g, '-');
  anchor = (value: string) => `#${this.toId(value)}`;

  mouseEnterHeader = (header: Header) => {
    const mousedOverHeaders = new Set(this.state.mousedOverHeaders);
    mousedOverHeaders.add(header);
    this.setState(() => ({ mousedOverHeaders }));
  }

  mouseLeaveMenu = () => this.setState(() => ({ mousedOverHeaders: new Set() }));

  SearchBar = () => (
    <Search
      size="mini"
      value={this.state.searchValue}
      onSearchChange={this.handleSearchChange}
      onKeyDown={this.onSearchKeyDown}
      open={false}
    />
  )

  renderSubMenu = (header: Header, isHeaderOpen: boolean): JSX.Element => (
    <Menu.Menu as={Collapse} isOpened={isHeaderOpen}>
      <React.Fragment>
        {header.children.map(({ value, index }) => (
          <Menu.Item
            key={value}
            active={this.props.active === index}
            as={HashLink}
            to={this.anchor(value)}
            onClick={e => e.stopPropagation()}
            scroll={scrollTo}
          >
            <span>{value}</span>
          </Menu.Item>
        ))}
      </React.Fragment>
    </Menu.Menu>
  )

  renderMenuItems = () => {
    const { searchValue, headers, mousedOverHeaders } = this.state;
    const { active } = this.props;
    const isSearching = searchValue.length >= MIN_CHARS;

    const isActive = (header: Header): boolean => active === header.index || header.children.some(isActive);

    return headers.map((header) => {
      const isHeaderActive = isActive(header);
      const hasBeenMousedOver = mousedOverHeaders.has(header);
      const isHeaderOpen = hasBeenMousedOver || isSearching || isHeaderActive;
      return (
        <Menu.Item key={header.value} active={isHeaderActive} onMouseEnter={() => this.mouseEnterHeader(header)}>
          <Accordion.Title
            as={HashLink}
            to={this.anchor(header.value)}
            scroll={scrollTo}
            active={isHeaderOpen}
            content={header.value}
          />
          {this.renderSubMenu(header, isHeaderOpen)}
        </Menu.Item>
      );
    });
  }

  Menu = () => (
    <Accordion
      as={Menu}
      inverted
      borderless
      fluid
      vertical
      size="small"
      id="DocMenu"
      onMouseLeave={this.mouseLeaveMenu}
      className="shadow"
    >
      {this.renderMenuItems()}
    </Accordion>
  )

  render() {
    const isLoading = this.props.markdown.length === 0;

    return (
      <React.Fragment>
        <this.SearchBar/>
        {!isLoading && <this.Menu/>}
      </React.Fragment>
    );
  }
}

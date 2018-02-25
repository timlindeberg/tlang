import * as React from 'react';
import { Divider, Segment, Header, List, Image } from 'semantic-ui-react';
import CodeBlock from 'components/CodeBlock';

interface DocumentationProps {
  markdown: AST[];
  setActive: (active: string) => void;
}

interface DocumentationState {
  active: string;
  documentation: JSX.Element[];
}

export enum Type {
  Root = 'root',
  Text = 'text',
  Paragraph = 'paragraph',
  Heading = 'heading',
  List = 'list',
  ListItem = 'listItem',
  ThematicBreak = 'thematicBreak',
  Link = 'link',
  Emphasis = 'emphasis',
  Strong = 'strong',
  InlineCode = 'inlineCode',
  Image = 'image',
  Table = 'table',
  Code = 'code',
}

export interface AST {
  type: Type;
  children: AST[];
  value?: string;
  depth?: number;
  ordered?: boolean;
  url?: string;
  alt?: string;
  lang?: string;
}

interface Header {
  name: string;
  ref: any;
}

export default class Documentation extends React.Component<DocumentationProps, DocumentationState> {

  ref: any = null;
  headers: Header[] = [];

  state: DocumentationState = { active: '', documentation: [] };

  componentDidMount() {
    this.setState(() => ({ documentation: this.createDocumentation(this.props) }));
  }

  componentWillReceiveProps(nextProps: DocumentationProps) {
    this.setState(() => ({ documentation: this.createDocumentation(nextProps) }));
  }

  componentWillUnmount() {
    if (this.ref) {
      this.ref.removeEventListener('scroll', this.onScroll);
    }
  }

  createDocumentation = (props: DocumentationProps): JSX.Element[] => {
    const markdown = props.markdown;

    let documentation: JSX.Element[] = [];
    markdown.forEach((ast, i) => {
      documentation = documentation.concat(this.astToReact(ast));
      if (i < markdown.length - 1) {
        documentation.push(<Divider style={{ marginTop: '2em', marginBottom: '2em' }}/>);
      }
    });
    return documentation;
  }

  astToReact = (ast: AST): JSX.Element => this.parse(ast) as JSX.Element;

  parse = ({ type, children, value, ...rest }: AST): JSX.Element | string => {
    const parse = this.parse;
    switch (type) {
    case Type.Root:
      return <React.Fragment>{children.map(parse)}</React.Fragment>;
    case Type.Text:
      return value!;
    case Type.Paragraph:
      return <p>{children.map(parse)}</p>;
    case Type.Heading:
      const child = parse(children[0]) as string;
      const depth = rest.depth!;
      return (
        <div ref={(r: any) => this.headers.push({ ref: r, name: child })} >
          <Header as={`h${depth}`} id={child.replace(/ /g, '-')}>{child}</Header>
        </div>
      );
    case Type.List:
      return <List bulleted={!rest.ordered!} ordered={rest.ordered!}>{children.map(parse)}</List>;
    case Type.ListItem:
      return <List.Item>{children.map((c: any) => parse(c.type === Type.Paragraph ? c.children[0] : c))}</List.Item>;
    case Type.ThematicBreak:
      return <Divider/>;
    case Type.Link:
      return <a href={rest.url!}>{children.map(parse)}</a>;
    case Type.Emphasis:
      return <em>{children.map(parse)}</em>;
    case Type.Strong:
      return <strong>{children.map(parse)}</strong>;
    case Type.InlineCode:
      return <code>{value!}</code>;
    case Type.Image:
      return <Image href={rest.url!}>{rest.alt!}</Image>;
    case Type.Table:
      return <p>TABLERERU</p>;
    case Type.Code:
      return <CodeBlock language={rest.lang!}>{value!}</CodeBlock>;
    default:
      throw new Error(`Unsupported type: ${type}`);
    }
  }

  onScroll = () => {
    const { name } = this.headerClosestToMiddle();
    if (name === this.state.active) {
      return;
    }

    this.setState(() => ({ active: name }));
    this.props.setActive(name);
  }

  headerClosestToMiddle = () => {
    const body = document.body;
    const html = document.documentElement;

    const height = Math.max(
      body.scrollHeight,
      body.offsetHeight,
      html.clientHeight,
      html.scrollHeight,
      html.offsetHeight
    );

    const distanceToMiddle = (ref: any) => {
      if (!ref) {
        return Infinity;
      }
      const middleOfRef = ref.getBoundingClientRect().top + (ref.clientHeight / 2);
      return Math.abs(middleOfRef - (height / 2));
    };

    return this.headers
      .sort(({ ref: refA }, { ref: refB }) => distanceToMiddle(refA) - distanceToMiddle(refB))[0];
  }

  divMounted = (ref: any) => {
    if (ref) {
      this.ref = ref;
      ref.addEventListener('scroll', this.onScroll);
    }
  }

  render() {
    return (
      <div ref={this.divMounted} style={{ height: '100vh', overflowY: 'scroll' }}>
        <Segment style={{ borderRadius: 0, padding: '1.5em' }}>
          {this.state.documentation}
        </Segment>
      </div>
    );
  }
}

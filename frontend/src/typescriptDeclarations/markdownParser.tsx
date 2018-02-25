import * as React from 'react';
import { Divider, Header, Image, List } from 'semantic-ui-react';
import CodeBlock from 'components/CodeBlock';

const astToReact = (ast: AST): JSX.Element => parse(ast) as JSX.Element;

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

const parse = ({ type, children, value, ...rest }: AST): JSX.Element | string => {
  switch (type) {
  case Type.Root:
    return <React.Fragment>{children.map(parse)}</React.Fragment>;
  case Type.Text:
    return value!;
  case Type.Paragraph:
    return <p>{children.map(parse)}</p>;
  case Type.Heading:
    const child = parse(children[0]) as String;
    return <Header as={`h${rest.depth!}`} id={child.replace(/ /g, '-')}>{child}</Header>;
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
};

export default astToReact;

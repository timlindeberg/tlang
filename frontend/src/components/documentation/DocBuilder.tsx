import { AST, Type } from 'types/markdown';
import { Divider, Header, List, Image, Segment, Table } from 'semantic-ui-react';
import * as React from 'react';
import CodeBlock from 'components/misc/CodeBlock';
import { decode } from 'he';

export interface Block {
  name: string;
  elements: JSX.Element[];
}

export default class DocBuilder {

  private elements: JSX.Element[];
  private id: string;
  private nodeCount: number;
  private currentBlock?: Block;
  private onBlockMounted: (ref: any) => void;

  constructor(id: string, onBlockMounted: (ref: any) => void) {
    this.id = id;
    this.nodeCount = 0;
    this.onBlockMounted = onBlockMounted;
    this.elements = [];
  }

  build = (ast: AST): JSX.Element => {
    ast.children.forEach((child) => {
      const element = this.parse(child) as JSX.Element;
      if (child.type === Type.Heading && (child.depth!) < 3) {
        this.addCurrentBlock();
        const name = this.parse(child.children[0]) as string;
        this.currentBlock = { name, elements: [] };
      }
      this.currentBlock!.elements.push(element);
    });
    this.addCurrentBlock();

    return <React.Fragment key={this.id}>{this.elements}</React.Fragment>;
  }

  private addCurrentBlock = () => {
    const block = this.currentBlock;
    if (!block) {
      return;
    }

    this.elements.push(
      <div key={block.name} className="Documentation-block" ref={this.onBlockMounted}>
        {block.elements}
      </div>
    );
  }

  private parse = (ast: AST): JSX.Element | string => {
    if (ast.type === Type.Text) {
      return decode(ast.value!);
    }

    return React.cloneElement(this.parseElement(ast), { key: this.nextKey() });
  }

  private nextKey = (): string => `${this.id} ${this.nodeCount++}`;

  private parseElement = ({ type, children, value, ...rest }: AST): JSX.Element => {
    const parse = this.parse;
    switch (type) {
    case Type.Paragraph:
      return <p>{children.map(parse)}</p>;
    case Type.Heading:
      const heading = parse(children[0]) as string;
      const depth = rest.depth!;
      const id = depth < 3 ? heading.replace(/ /g, '-') : undefined;
      return <Header as={`h${depth}`} id={id}>{heading}</Header>;
    case Type.List:
      return <List bulleted={!rest.ordered!} ordered={rest.ordered!}>{children.map(parse)}</List>;
    case Type.ListItem:
      return <List.Item>{children.map(parse)}</List.Item>;
    case Type.ThematicBreak:
      return <Divider/>;
    case Type.Link:
      return <a href={rest.url!}>{children.map(parse)}</a>;
    case Type.Emphasis:
      return <em>{children.map(parse)}</em>;
    case Type.Strong:
      return <strong>{children.map(parse)}</strong>;
    case Type.InlineCode:
      return <code>{decode(value!)}</code>;
    case Type.Image:
      return <Image href={rest.url!}>{rest.alt!}</Image>;
    case Type.Table:
      const headerItems = children[0].children;
      const rows = children.slice(1);
      return (
        <Table celled selectable striped collapsing className="shadow-hover">
          <Table.Header>
            <Table.Row>
              {headerItems.map(c => <Table.HeaderCell key={this.nextKey()}>{c.children.map(parse)}</Table.HeaderCell>)}
            </Table.Row>
          </Table.Header>
          <Table.Body>
            {rows.map(row => <Table.Row key={this.nextKey()}>{row.children.map(parse)}</Table.Row>)}
          </Table.Body>
        </Table>
      );
    case Type.TableCell:
      return <Table.Cell>{children.map(parse)}</Table.Cell>;
    case Type.Code:
      return <CodeBlock language={rest.lang!}>{decode(value!)}</CodeBlock>;
    case Type.BlockQuote:
      return <Segment>{children.map(parse)}</Segment>;
    default:
      throw new Error(`Unsupported type: ${type}`);
    }
  }

}

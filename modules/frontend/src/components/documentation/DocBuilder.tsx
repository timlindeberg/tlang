import CodeBlock from 'components/misc/CodeBlock';
import { decode } from 'he';
import * as React from 'react';
import { Divider, Header, Image, List, Segment, Table } from 'semantic-ui-react';
import { AST, Markdown } from 'types/markdown';

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
      if (child.type === Markdown.Heading && (child.depth!) < 3) {
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
    if (ast.type === Markdown.Text) {
      return decode(ast.value!);
    }

    return React.cloneElement(this.parseElement(ast), { key: this.nextKey() });
  }

  private nextKey = (): string => `${this.id} ${this.nodeCount++}`;

  private parseElement = ({ type, children, value, ...rest }: AST): JSX.Element => {
    const parse = this.parse;
    switch (type) {
    case Markdown.Paragraph:
      return <p>{children.map(parse)}</p>;
    case Markdown.Heading:
      const heading = parse(children[0]) as string;
      const depth = rest.depth!;
      const id = depth < 3 ? heading.replace(/ /g, '-') : undefined;
      return <Header as={`h${depth}`} id={id}>{heading}</Header>;
    case Markdown.List:
      return <List bulleted={!rest.ordered!} ordered={rest.ordered!}>{children.map(parse)}</List>;
    case Markdown.ListItem:
      return <List.Item>{children.map(parse)}</List.Item>;
    case Markdown.ThematicBreak:
      return <Divider/>;
    case Markdown.Link:
      return <a href={rest.url!}>{children.map(parse)}</a>;
    case Markdown.Emphasis:
      return <em>{children.map(parse)}</em>;
    case Markdown.Strong:
      return <strong>{children.map(parse)}</strong>;
    case Markdown.InlineCode:
      return <code>{decode(value!)}</code>;
    case Markdown.Image:
      return <Image href={rest.url!}>{rest.alt!}</Image>;
    case Markdown.Table:
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
    case Markdown.TableCell:
      return <Table.Cell>{children.map(parse)}</Table.Cell>;
    case Markdown.Code:
      return <CodeBlock language={rest.lang!}>{decode(value!)}</CodeBlock>;
    case Markdown.BlockQuote:
      return <Segment>{children.map(parse)}</Segment>;
    default:
      throw new Error(`Unsupported type: ${type}`);
    }
  }

}

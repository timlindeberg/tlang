import { AST, Type } from 'types/markdown';
import { Divider, Header, List, Image } from 'semantic-ui-react';
import * as React from 'react';
import CodeBlock from 'components/CodeBlock';

export interface Block {
  name: string;
  elements: JSX.Element[];
}

export default class DocBuilder {

  private elements: JSX.Element[];
  private id: string;
  private nodeCount: number;
  private currentBlock?: Block;
  private onBlockMounted: (ref: any, block: Block) => void;

  constructor(id: string, onBlockMounted: (ref: any, block: Block) => void) {
    this.id = id;
    this.nodeCount = 0;
    this.onBlockMounted = onBlockMounted;
    this.elements = [];
  }

  build = (ast: AST): JSX.Element => {
    ast.children.forEach((child) => {
      const element = this.parse(child) as JSX.Element;
      if (child.type === Type.Heading && (child.depth!) < 3) {
        this.addBlock();
        const name = this.parse(child.children[0]) as string;
        this.currentBlock = { name, elements: [] };
      }
      this.currentBlock!.elements.push(element);
    });
    this.addBlock();

    return <React.Fragment key={this.id}>{this.elements}</React.Fragment>;
  }

  private addBlock = () => {
    const block = this.currentBlock;
    if (!block) {
      return;
    }

    this.elements.push(
      <div
        key={block.name}
        className="Documentation-block"
        ref={(r: any) => this.onBlockMounted(r, block)}
      >
        {block.elements}
      </div>
    );
  }

  private parse = (ast: AST): JSX.Element | string => {
    if (ast.type === Type.Text) {
      return ast.value!;
    }

    const key = `${this.id} ${this.nodeCount++}`;
    return React.cloneElement(this.parseElement(ast), { key });
  }

  private parseElement = ({ type, children, value, ...rest }: AST): JSX.Element => {
    const parse = this.parse;
    switch (type) {
    case Type.Paragraph:
      return <p>{children.map(parse)}</p>;
    case Type.Heading:
      const heading = parse(children[0]) as string;
      return <Header as={`h${rest.depth!}`} id={heading.replace(/ /g, '-')}>{heading}</Header>;
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

}

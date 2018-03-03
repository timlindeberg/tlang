import * as React from 'react';
import { Divider, Segment } from 'semantic-ui-react';
import { AST } from 'types/markdown';
import DocBuilder, { Block } from 'components/documentation/DocBuilder';

interface DocumentationProps {
  markdown: AST[];
  setActive: (active: string) => void;
}

interface DocumentationState {
  documentation: JSX.Element[];
}

export default class Documentation extends React.Component<DocumentationProps, DocumentationState> {

  state: DocumentationState = { documentation: [] };

  ref?: Element;
  blocks: { [s: string]: Element } = {};

  componentDidMount() {
    this.setState(() => ({ documentation: this.createDocumentation(this.props) }));
  }

  componentWillReceiveProps(nextProps: DocumentationProps) {
    if (nextProps.markdown !== this.props.markdown) {
      this.setState(() => ({ documentation: this.createDocumentation(nextProps) }));
    }
  }

  shouldComponentUpdate(nextProps: DocumentationProps, nextState: DocumentationState) {
    return nextProps.markdown !== this.props.markdown || nextState.documentation !== this.state.documentation;
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
      const docBuilder = new DocBuilder(i.toString(), this.onBlockMounted);
      documentation = documentation.concat(docBuilder.build(ast));
      if (i < markdown.length - 1) {
        documentation.push(<Divider key={`d${i}`} style={{ marginTop: '2em', marginBottom: '2em' }}/>);
      }
    });
    return documentation;
  }

  onBlockMounted = (ref: any, block: Block): void => this.blocks[block.name] = ref;

  onScroll = (): void => {
    const name = this.headerClosestToMiddle();
    this.props.setActive(name);
  }

  headerClosestToMiddle = (): string => {
    const body = document.body;
    const html = document.documentElement;

    const height = Math.max(
      body.scrollHeight,
      body.offsetHeight,
      html.scrollHeight,
      html.offsetHeight
    );

    const distanceToMiddle = (ref: Element): number => {
      if (!ref) {
        return Infinity;
      }
      const middleOfRef = ref.getBoundingClientRect().top + (ref.clientHeight / 2);
      return Math.abs(middleOfRef - (height / 2));
    };

    return Object.keys(this.blocks)
      .map((key): [string, number] => [key, distanceToMiddle(this.blocks[key])])
      .sort(([_1, d1], [_2, d2]) => d1 - d2)
      [0][0];
  }

  divMounted = (ref: any) => {
    if (ref) {
      this.ref = ref;
      ref.addEventListener('scroll', this.onScroll);
    }
  }

  render() {
    return (
      <div ref={this.divMounted} id="Documentation-docs">
        <Segment style={{ border: 'none' }}>
          {this.state.documentation}
        </Segment>
      </div>
    );
  }
}

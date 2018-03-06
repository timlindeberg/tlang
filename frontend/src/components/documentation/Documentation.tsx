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

  static NAV_BAR_HEIGHT = 5;
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
    if (markdown.length === 0) {
      return documentation;
    }

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

    const height = Math.max(body.scrollHeight, body.offsetHeight, html.scrollHeight, html.offsetHeight);
    const headerSize = Documentation.NAV_BAR_HEIGHT * parseFloat(getComputedStyle(document.documentElement)!.fontSize!);

    const viewHeight = height - headerSize;

    const percentageOfViewArea = (ref: Element): number => {
      if (!ref) {
        return Infinity;
      }

      const rect = ref.getBoundingClientRect();
      if (rect.bottom < headerSize || rect.top > height) {
        return 0;
      }

      const top = Math.max(headerSize, rect.top);
      const bottom = Math.min(height, rect.bottom);

      return (bottom - top) / viewHeight;
    };

    let highestPercentage = 0;
    let mostVisibleBlock = '';
    Object.keys(this.blocks).forEach((block) => {
      const percentage = percentageOfViewArea(this.blocks[block]);
      if (percentage > highestPercentage) {
        highestPercentage = percentage;
        mostVisibleBlock = block;
      }
    });

    return mostVisibleBlock;
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

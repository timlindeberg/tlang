import * as React from 'react';
import { Divider, Segment } from 'semantic-ui-react';
import { AST } from 'types/markdown';
import DocBuilder from 'components/documentation/DocBuilder';

interface DocumentationProps {
  markdown: AST[];
  active: number;
  setActive: (active: number) => void;
}

interface DocumentationState {
  documentation: JSX.Element[];
}

export default class Documentation extends React.Component<DocumentationProps, DocumentationState> {

  static NAV_BAR_HEIGHT = 5;
  static SCROLL_OFFSET = 5;

  state: DocumentationState = { documentation: [] };

  private ref?: Element;
  private navBarHeight: number = 0;
  private blocks: Element[] = [];
  private lastScrollPosition: number = 0;

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

  onBlockMounted = (ref: any): void => { this.blocks.push(ref); };

  onScrollUp = (): void => {
    const { setActive, active } = this.props;

    if (active === this.blocks.length - 1) {
      return;
    }
    const block = this.blocks[active + 1];
    const pos = block.getBoundingClientRect().top;
    if (pos <= this.navBarHeight + Documentation.SCROLL_OFFSET) {
      setActive(active + 1);
    }
  }

  onScrollDown = (): void => {
    const { setActive, active } = this.props;

    if (active === 0) {
      return;
    }
    const block = this.blocks[active];
    const pos = block.getBoundingClientRect().top;
    if (pos >= this.navBarHeight + Documentation.SCROLL_OFFSET) {
      setActive(active - 1);
    }
  }

  onScroll = (): void => {
    const scrollPos = this.ref!.scrollTop;
    if (scrollPos > this.lastScrollPosition) {
      this.onScrollUp();
    } else {
      this.onScrollDown();
    }
    this.lastScrollPosition = scrollPos;
  }

  divMounted = (ref: any) => {
    if (ref) {
      this.ref = ref;
      this.navBarHeight = Documentation.NAV_BAR_HEIGHT *
        parseFloat(getComputedStyle(document.documentElement)!.fontSize!);
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

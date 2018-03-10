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

  static NAV_BAR_HEIGHT_EM = 5;
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

  onScrollDown = (): number => {
    const { active } = this.props;

    let activeBlock = active;
    for (let block = active; block < this.blocks.length - 1; block++) {
      const pos = this.blocks[block + 1].getBoundingClientRect().top;
      if (pos > this.navBarHeight + Documentation.SCROLL_OFFSET) {
        break;
      }
      activeBlock = block + 1;
    }
    return activeBlock;
  }

  onScrollUp = (): number => {
    const { active } = this.props;

    let activeBlock = active;
    for (let block = active; block >= 1; block--) {
      const pos = this.blocks[block].getBoundingClientRect().top;
      if (pos < this.navBarHeight + Documentation.SCROLL_OFFSET) {
        break;
      }
      activeBlock = block - 1;
    }
    return activeBlock;
  }

  onScroll = (): void => {
    const { setActive, active } = this.props;

    const scrollPos = this.ref!.scrollTop;
    const activeBlock = scrollPos > this.lastScrollPosition ? this.onScrollDown()  : this.onScrollUp();
    if (activeBlock !== active) {
      setActive(activeBlock);
    }
    this.lastScrollPosition = scrollPos;
  }

  divMounted = (ref: any) => {
    if (!ref) {
      return;
    }

    this.ref = ref;
    const fontSize = getComputedStyle(document.documentElement)!.fontSize!;
    this.navBarHeight = Documentation.NAV_BAR_HEIGHT_EM * parseFloat(fontSize);
    ref.addEventListener('scroll', this.onScroll);
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

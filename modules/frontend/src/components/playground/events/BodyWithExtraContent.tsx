import * as React from 'react';
import { Collapse } from 'react-collapse';
import Divider from 'semantic-ui-react/dist/commonjs/elements/Divider/Divider';
import Accordion from 'semantic-ui-react/dist/commonjs/modules/Accordion/Accordion';
import { thousandSeperatedNumber } from 'utils/misc';

interface BodyWithExtraContentProps {
  numLines: number;
  width: number;
  makeLine: (i: number) => JSX.Element;
  type: string;
  maxToShow: number;
}

interface BodyWithExtraContentState {
  isOpen: boolean;
}

export default class BodyWithExtraContent
  extends React.Component<BodyWithExtraContentProps, BodyWithExtraContentState> {

  state: BodyWithExtraContentState = {
    isOpen: false,
  };

  toggleOpen = () => this.setState(state => ({ isOpen: !state.isOpen }));

  getToggleText = (isOpen: Boolean, length: number): string =>
    isOpen ? 'Show less' : `Show ${thousandSeperatedNumber(length)} more ${this.props.type}${length > 1 ? 's' : ''}`

  makeFirst = () => {
    const { maxToShow, makeLine, numLines } = this.props;
    const length = Math.min(maxToShow, numLines);
    return Array.apply(null, { length }).map((_: any, i: number) => makeLine(i));
  }

  makeExtra = () => {
    const { maxToShow, makeLine, numLines } = this.props;
    const length = numLines - maxToShow;
    return Array.apply(null, { length }).map((_: any, i: number) => makeLine(maxToShow + i));
  }

  render() {
    const { numLines, maxToShow } = this.props;
    const { isOpen } = this.state;
    return (
      <div className="result-block">
        {this.makeFirst()}
        {numLines > maxToShow && (
          <React.Fragment>
            <Collapse isOpened={isOpen}>{isOpen && this.makeExtra()}</Collapse>
            <Divider/>
            <Accordion>
              <Accordion.Title
                active={isOpen}
                content={this.getToggleText(isOpen, numLines - maxToShow)}
                onClick={this.toggleOpen}
                className="show-more"
              />
            </Accordion>
          </React.Fragment>
        )}
      </div>
    );
  }
}

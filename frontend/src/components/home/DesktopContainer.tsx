import * as React from 'react';
import { Responsive, Segment, Visibility } from 'semantic-ui-react';
import Heading from 'components/home/Heading';
import Navbar from 'components/layout/Navbar';

interface DesktopContainerProps {
  children?: any;
}

interface DesktopContainerState {
  fixed: boolean;
}

export default class DesktopContainer extends React.Component<DesktopContainerProps, DesktopContainerState> {
  state = { fixed: false };

  hideFixedMenu = () => this.setState({ fixed: false });
  showFixedMenu = () => this.setState({ fixed: true });

  render() {
    const { children } = this.props;

    return (
      <Responsive {...Responsive.onlyComputer}>
        <Visibility once={false} onBottomPassed={this.showFixedMenu} onBottomPassedReverse={this.hideFixedMenu}>
          <Segment textAlign="center" vertical inverted id="Heading-segment">
            <Navbar />
            <Heading mobile={false}/>
          </Segment>
        </Visibility>
        {children}
      </Responsive>
    );
  }
}

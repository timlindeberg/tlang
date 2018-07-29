import Heading from 'components/home/Heading';
import Navbar from 'components/layout/Navbar';
import * as React from 'react';
import { Container, Responsive, Segment, Visibility } from 'semantic-ui-react';

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
            <Container>
              <Navbar className="animated fade-in-right"/>
            </Container>
            <Heading mobile={false}/>
          </Segment>
        </Visibility>
        {children}
      </Responsive>
    );
  }
}

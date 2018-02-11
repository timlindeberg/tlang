import * as React from 'react';
import { Container, Menu, Responsive, Segment, Visibility } from 'semantic-ui-react';
import Heading from 'components/home/Heading';

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
    const { fixed } = this.state;

    return (
        <Responsive {...Responsive.onlyComputer}>
             <Visibility once={false} onBottomPassed={this.showFixedMenu} onBottomPassedReverse={this.hideFixedMenu}>
                <Segment textAlign="center" vertical id="Heading-container">
                    <Menu
                        fixed={fixed ? 'top' : undefined}
                        inverted={!fixed}
                        pointing={!fixed}
                        secondary={!fixed}
                        size="large"
                    >
                        <Container>
                            <Menu.Item as="a" active>Home</Menu.Item>
                            <Menu.Item as="a">Documentation</Menu.Item>
                            <Menu.Item as="a">Playground</Menu.Item>
                        </Container>
                    </Menu>
                    <Heading mobile={false}/>
                </Segment>
            </Visibility>

            {children}
        </Responsive>
    );
  }
}
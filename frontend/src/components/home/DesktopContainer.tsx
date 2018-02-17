import * as React from 'react';
import { Link, withRouter, RouteComponentProps } from 'react-router-dom';
import { Container, Menu, Responsive, Segment, Visibility } from 'semantic-ui-react';
import Heading from 'components/home/Heading';

interface DesktopContainerProps {
  children?: any;
}

interface DesktopContainerState {
  fixed: boolean;
}

interface MenuLinkProps {
  children: React.ReactNode;
  to: string;
}

const MenuLink = withRouter(({ children, to, location }: RouteComponentProps<any> & MenuLinkProps) => (
  <Menu.Item active={location.pathname === to} as={Link} to={to}>{children}</Menu.Item>
));

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
          <Segment textAlign="center" vertical inverted id="Heading-segment">
            <Menu
              fixed={fixed ? 'top' : undefined}
              inverted={!fixed}
              pointing={!fixed}
              secondary={!fixed}
              size="large"
            >
              <Container>
                <MenuLink to="/">Home</MenuLink>
                <MenuLink to="/documentation">Documentation</MenuLink>
                <MenuLink to="/get_started">Get Started</MenuLink>
                <MenuLink to="/playground">Playground</MenuLink>
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
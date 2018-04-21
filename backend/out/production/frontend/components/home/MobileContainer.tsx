import * as React from 'react';
import { Button, Container, Icon, Menu, Responsive, Segment, Sidebar } from 'semantic-ui-react';
import Heading from 'components/home/Heading';

interface MobileContainerProps {
  children: any;
}

interface MobileContainerState {
  sidebarOpened: boolean;
}

export default class MobileContainer extends React.Component<MobileContainerProps, MobileContainerState> {
  state = { sidebarOpened: false };

  handleToggle = () => this.setState({ sidebarOpened: !this.state.sidebarOpened });

  render() {
    const { children } = this.props;
    const { sidebarOpened } = this.state;

    return (
      <Responsive {...Responsive.onlyMobile}>
        <Sidebar.Pushable>
          <Sidebar as={Menu} animation="uncover" inverted vertical visible={sidebarOpened}>
            <Menu.Item as="a" active>Home</Menu.Item>
            <Menu.Item as="a">Work</Menu.Item>
            <Menu.Item as="a">Company</Menu.Item>
            <Menu.Item as="a">Careers</Menu.Item>
            <Menu.Item as="a">Log in</Menu.Item>
            <Menu.Item as="a" primary>Sign Up</Menu.Item>
          </Sidebar>

          <Sidebar.Pusher dimmed={sidebarOpened} onClick={this.handleToggle} style={{ minHeight: '100vh' }}>
            <Segment inverted textAlign="center" style={{ minHeight: 350, padding: '1em 0em' }} vertical>
              <Container>
                <Menu inverted pointing secondary size="large">
                  <Menu.Item onClick={this.handleToggle}>
                    <Icon name="sidebar"/>
                  </Menu.Item>
                  <Menu.Item position="right">
                    <Button as="a" inverted>Log in</Button>
                    <Button as="a" inverted style={{ marginLeft: '0.5em' }}>Sign Up</Button>
                  </Menu.Item>
                </Menu>
              </Container>
              <Heading mobile/>
            </Segment>

            {children}
          </Sidebar.Pusher>
        </Sidebar.Pushable>
      </Responsive>
    );
  }
}
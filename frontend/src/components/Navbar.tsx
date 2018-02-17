import * as React from 'react';
import { Container, Menu } from 'semantic-ui-react';

interface NavbarProps {

}

interface NavbarState {

}

export default class Navbar extends React.Component<NavbarProps, NavbarState> {
  state = { fixed: false };

  render() {
    const { fixed } = this.state;

    return (
      <Menu
        fixed={fixed ? 'top' : undefined}
        inverted={!fixed}
        pointing={!fixed}
        secondary={!fixed}
        size="large"
      >
        <Container>
          <Menu.Item as="a" active>Home</Menu.Item>
          <Menu.Item as="a">Get started</Menu.Item>
          <Menu.Item as="a">Documentation</Menu.Item>
          <Menu.Item as="a">Playground</Menu.Item>
        </Container>
      </Menu>
    );
  }

}
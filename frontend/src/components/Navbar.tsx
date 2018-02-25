import * as React from 'react';
import { Menu, Container } from 'semantic-ui-react';
import { Link } from "react-router-dom";
import { RouteComponentProps, withRouter } from 'react-router';

interface MenuLinkProps {
  children: React.ReactNode;
  to: string;
}

const MenuLink = withRouter(({ children, to, location }: RouteComponentProps<any> & MenuLinkProps) => (
  <Menu.Item active={location.pathname === to} as={Link} to={to}>{children}</Menu.Item>
));

const Navbar = () => (
  <Container>
    <Menu secondary inverted pointing size="large">
      <MenuLink to="/">Home</MenuLink>
      <MenuLink to="/documentation">Documentation</MenuLink>
      <MenuLink to="/get_started">Get started</MenuLink>
      <MenuLink to="/playground">Playground</MenuLink>
    </Menu>
  </Container>
);

export default Navbar;

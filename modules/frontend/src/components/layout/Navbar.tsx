import * as React from 'react';
import { RouteComponentProps, withRouter } from 'react-router';
import { Link } from 'react-router-dom';
import { Menu } from 'semantic-ui-react';

interface MenuLinkProps {
  children: React.ReactNode;
  to: string;
}

const MenuLink = withRouter(({ children, to, location }: RouteComponentProps<any> & MenuLinkProps) => (
  <Menu.Item active={location.pathname === to} as={Link} to={to}>{children}</Menu.Item>
));

const Navbar = ({ className }: {className?: string}) => (
  <div className={className}>
    <Menu secondary inverted pointing size="large" className="shadow">
      <MenuLink to="/">Home</MenuLink>
      <MenuLink to="/documentation">Documentation</MenuLink>
      <MenuLink to="/getting_started">Get started</MenuLink>
      <MenuLink to="/playground">Playground</MenuLink>
    </Menu>
  </div>
);

export default Navbar;

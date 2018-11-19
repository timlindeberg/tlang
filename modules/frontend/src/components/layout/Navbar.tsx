import Layout from 'components/layout/Layout';
import * as React from 'react';
import { RouteComponentProps, withRouter } from 'react-router';
import { Link } from 'react-router-dom';
import Menu from 'semantic-ui-react/dist/commonjs/collections/Menu/Menu';

interface NavbarProps {
  className?: string;
}

interface MenuLinkProps {
  children: React.ReactNode;
  to: string;
}

type SemanticUISize = 'mini' | 'tiny' | 'small' | 'large' | 'huge' | 'massive';

const MenuLink = withRouter(({ children, to, location }: RouteComponentProps<any> & MenuLinkProps) => (
  <Menu.Item active={location.pathname === to} as={Link} to={to}>{children}</Menu.Item>
));

const menu = (size: SemanticUISize, className?: string) => (
  <div className={className}>
    <Menu secondary inverted pointing size={size}>
      <MenuLink to="/">Home</MenuLink>
      <MenuLink to="/documentation">Documentation</MenuLink>
      <MenuLink to="/getting_started">Get started</MenuLink>
      <MenuLink to="/playground">Playground</MenuLink>
    </Menu>
  </div>
);

const Navbar = ({ className }: NavbarProps) => (
  <Layout mobile={() => menu('tiny', className)} desktop={() => menu('large', className)} />
);

export default Navbar;

import * as React from 'react';
import { Grid, Header, Menu, Segment } from 'semantic-ui-react';
import { Link, withRouter, RouteComponentProps } from 'react-router-dom';

import 'components/home/HomeView.scss';


interface MenuLinkProps {
  children: React.ReactNode;
  to: string;
}

const MenuLink = withRouter(({ children, to, location }: RouteComponentProps<any> & MenuLinkProps) => (
  <Menu.Item active={location.pathname === to} as={Link} to={to}>{children}</Menu.Item>
));

const DocumentationView = () => (
  <Grid stackable verticalAlign="middle"  >
    <Grid.Column width={4}>
      <Menu
        fluid
        vertical
        size="mini"
      >
        <Menu.Header>Navigation</Menu.Header>
        <MenuLink to="/">Home</MenuLink>
        <MenuLink to="/documentation">Documentation</MenuLink>
        <MenuLink to="/get_started">Get Started</MenuLink>
        <MenuLink to="/playground">Playground</MenuLink>
      </Menu>
    </Grid.Column>

    <Grid.Column width={12}>
      <Segment>
        <Header as="h1">This is the documentation</Header>
      </Segment>
    </Grid.Column>
  </Grid>
);

export default DocumentationView;

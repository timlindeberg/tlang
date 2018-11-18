import Navbar from 'components/layout/Navbar';
import Logo from 'components/misc/Logo';
import * as React from 'react';
import Grid from 'semantic-ui-react/dist/commonjs/collections/Grid/Grid';
import Segment from 'semantic-ui-react/dist/commonjs/elements/Segment/Segment';

const NavbarWithLogo = () => (
  <Segment textAlign="left" inverted id="MenuLayout-navbar">
    <Grid>
      <Grid.Column className="logo-fix">
        <Logo size={2.5}/>
      </Grid.Column>
      <Grid.Column>
        <Navbar/>
      </Grid.Column>
    </Grid>
  </Segment>
);

export default NavbarWithLogo;

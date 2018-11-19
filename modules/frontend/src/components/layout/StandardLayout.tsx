import Footer from 'components/layout/Footer';
import Layout from 'components/layout/Layout';
import Navbar from 'components/layout/Navbar';
import Logo from 'components/misc/Logo';
import * as React from 'react';
import Grid from 'semantic-ui-react/dist/commonjs/collections/Grid/Grid';
import Segment from 'semantic-ui-react/dist/commonjs/elements/Segment/Segment';

interface StandardLayoutProps {
  bottom: boolean;
  children: React.ReactNode;
}

const layout = (logoSize: number, columnWidth: string, { bottom, children }: StandardLayoutProps) => (
  <React.Fragment>
    <Segment textAlign="left" inverted id="MenuLayout-navbar">
      <Grid>
        <Grid.Column style={{ width: columnWidth }}>
          <Logo size={logoSize}/>
        </Grid.Column>
        <Grid.Column>
          <Navbar />
        </Grid.Column>
      </Grid>
    </Segment>
    {children}
    <Footer bottom={bottom} />
  </React.Fragment>
);

const StandardLayout = (props: StandardLayoutProps) => (
  <Layout
    mobile={() => layout(2, '4.5em', props)}
    desktop={() => layout(2.5, '5.7em', props)}
  />
);

export default StandardLayout;

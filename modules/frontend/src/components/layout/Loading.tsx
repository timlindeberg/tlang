import 'components/gettingStarted/GettingStartedView.less';
import Footer from 'components/layout/Footer';
import 'components/layout/Loading.less';
import Navbar from 'components/layout/Navbar';
import Logo from 'components/misc/Logo';
import * as React from 'react';
import Grid from 'semantic-ui-react/dist/commonjs/collections/Grid/Grid';
import Segment from 'semantic-ui-react/dist/commonjs/elements/Segment/Segment';

export default class Loading extends React.Component<{}, {}> {

  render() {
    return (
      <React.Fragment>
        <Segment textAlign="left" inverted id="MenuLayout-navbar">
          <Grid>
            <Grid.Column>
              <Logo size={2.5}/>
            </Grid.Column>
            <Grid.Column>
              <Navbar/>
            </Grid.Column>
          </Grid>
        </Segment>
        <Segment id="GettingStartedView-content">
          <div className="loading-container"    >
            <h3 className="loading-text">Loading</h3>
            <div className="loading-bars">
              <div className="rect1"/>
              <div className="rect2"/>
              <div className="rect3"/>
              <div className="rect4"/>
              <div className="rect5"/>
              <div className="rect6"/>
            </div>
          </div>
        </Segment>
        <Footer bottom />
      </React.Fragment>
    );
  }
}

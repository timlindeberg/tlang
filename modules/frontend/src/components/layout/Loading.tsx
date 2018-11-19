import 'components/gettingStarted/GettingStartedView.less';
import 'components/layout/Loading.less';
import StandardLayout from 'components/layout/StandardLayout';
import * as React from 'react';
import Segment from 'semantic-ui-react/dist/commonjs/elements/Segment/Segment';

export default class Loading extends React.Component<{}, {}> {

  render() {
    return (
      <StandardLayout bottom>
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
      </StandardLayout>
    );
  }
}

import 'components/layout/Loading.less';
import * as React from 'react';
import Loader from 'semantic-ui-react/dist/commonjs/elements/Loader/Loader';

export default class Loading extends React.Component<{}, {}> {

  render() {
    return (
      <React.Fragment>
        <div className="loading-container">
          <h3 className="loading-text">Loading</h3>
          <Loader active />
          <div className="loading-bars">
            <div className="rect1"/>
            <div className="rect2"/>
            <div className="rect3"/>
            <div className="rect4"/>
            <div className="rect5"/>
            <div className="rect6"/>
          </div>
        </div>
      </React.Fragment>
    );
  }
}

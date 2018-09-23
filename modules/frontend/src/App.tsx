import DocumentationView from 'components/documentation/DocumentationView';
import GettingStartedView from 'components/gettingStarted/GettingStartedView';
import HomeView from 'components/home/HomeView';
import ScrollToTop from 'components/misc/ScrollToTop';
import PlayGroundView from 'components/playground/PlaygroundView';
import * as React from 'react';
import { BrowserRouter, Route, Switch } from 'react-router-dom';

class App extends React.Component {

  render() {
    return (
      <BrowserRouter>
        <ScrollToTop>
          <Switch>
            <Route exact path="/" component={HomeView}/>
            <Route exact path="/documentation" component={DocumentationView}/>
            <Route exact path="/playground" component={PlayGroundView}/>
            <Route exact path="/getting_started" component={GettingStartedView}/>
          </Switch>
        </ScrollToTop>
      </BrowserRouter>
    );
  }

}

export default App;

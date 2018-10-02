import HomeView from 'components/home/HomeView';
import ScrollToTop from 'components/misc/ScrollToTop';
import * as React from 'react';
import { BrowserRouter, Route, Switch } from 'react-router-dom';
import { makeLoadable } from 'utils/misc';

class App extends React.Component {

  private Documentation = makeLoadable(() => import(
    /* webpackChunkName: "documentation" */ 'components/documentation/DocumentationView')
  );
  private Playground = makeLoadable(() => import(
    /* webpackChunkName: "playground" */ 'components/playground/PlaygroundView')
  );
  private GettingStarted = makeLoadable(() => import(
    /* webpackChunkName: "getting_started" */'components/gettingStarted/GettingStartedView')
  );

  render() {
    return (
      <BrowserRouter>
        <ScrollToTop>
          <Switch>
            <Route exact path="/" component={HomeView}/>
            <Route exact path="/documentation" component={this.Documentation}/>
            <Route exact path="/playground" component={this.Playground}/>
            <Route exact path="/getting_started" component={this.GettingStarted}/>
          </Switch>
        </ScrollToTop>
      </BrowserRouter>
    );
  }

}

export default App;

import * as React from 'react';
import { BrowserRouter, Route, Switch } from 'react-router-dom';
import HomeView from 'components/home/HomeView';
import DocumentationView from 'components/documentation/DocumentationView';
import PlayGroundView from 'components/playground/PlaygroundView';

class App extends React.Component {

  render() {
    return (
      <BrowserRouter>
        <Switch>
          <Route exact path="/" component={HomeView}/>
          <Route exact path="/documentation" component={DocumentationView}/>
          <Route exact path="/playground" component={PlayGroundView}/>
        </Switch>
      </BrowserRouter>
    );
  }

}

export default App;

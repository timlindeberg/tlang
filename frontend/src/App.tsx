import * as React from 'react';
import HomeView from 'components/home/HomeView';
import { BrowserRouter, Route, Switch } from 'react-router-dom';
import DocumentationView from 'components/documentation/DocumentationView';

class App extends React.Component {

  render() {
    return (
      <BrowserRouter>
        <Switch>
          <Route exact path="/" component={HomeView} />
          <Route exact path="/documentation" component={DocumentationView} />
        </Switch>
      </BrowserRouter>
    );
  }

}

export default App;

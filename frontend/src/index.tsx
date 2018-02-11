import * as React from 'react';
import * as ReactDOM from 'react-dom';
import 'index.scss';
import 'semantic/dist/semantic.min.css';
import App from 'App';

import registerServiceWorker from  'registerServiceWorker';

ReactDOM.render(
  <App />,
  document.getElementById('root') as HTMLElement
);

registerServiceWorker();

if (module.hot) {
  module.hot.accept();
}
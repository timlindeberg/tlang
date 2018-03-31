import * as React from 'react';
import * as ReactDOM from 'react-dom';
import 'main.scss';
import 'semantic/dist/semantic.min.css';
import App from 'App';

import registerServiceWorker from  'registerServiceWorker';

const startHMRGroup = () => {
  const count: number = parseInt(sessionStorage.getItem('hmr_counter') || '0', 10);
  console.groupEnd();
  console.group(`━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━🔥 HMR ${count} 🔥━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━`);
  sessionStorage.setItem('hmr_counter', (count + 1).toString());
};

if (module.hot) {
  startHMRGroup();
  module.hot.accept();
}

ReactDOM.render(
  <App />,
  document.getElementById('root') as HTMLElement
);

registerServiceWorker();

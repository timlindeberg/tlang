import * as React from 'react';

import { Segment } from 'semantic-ui-react';
import Navbar from 'components/Navbar';
import * as ReactCodeMirror from 'react-codemirror';
import 'codemirror/lib/codemirror.css';
import 'utils/tlangSyntaxDefinition';

interface PlayGroundViewState {
}

const codeExample =
  `package t::lang

val helloWorld = new HelloWorld<Long>(1300L) + 
   new HelloWorld<Long>(37L)

helloWorld.Print() // prints "Hello world!" 1337 times

class HelloWorld<T> =
    
	var times: T
    
	Def new(times: T?) = 
		this.times = times ?: 1
    
	Def Print() = 
		for(var i = 0; i < times; i++)
			println("Hello world!")
            
	Def +(lhs: HelloWorld<T>, rhs: HelloWorld<T>) =
		new HelloWorld(lhs.times + rhs.times)`;

export default class PlayGroundView extends React.Component<{}, PlayGroundViewState> {

  state: PlayGroundViewState = {};

  render() {
    return (
      <React.Fragment>
        <Segment inverted>
          <Navbar/>
        </Segment>
        <ReactCodeMirror
          value={codeExample}
          options={{
            lineNumbers: true,
            mode: 'tlang',
            theme: 'tlang',
            undoDepth: 200,
            indentWithTabs: true,
            tabSize: 4,
            indentUnit: 4,
            showCursorWhenSelecting: true,
          }}
        />
      </React.Fragment>
    );
  }
}

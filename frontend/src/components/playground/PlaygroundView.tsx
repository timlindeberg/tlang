import * as React from 'react';

import * as ReactCodeMirror from 'react-codemirror';
import { Segment, Divider, Button, Icon, Grid, Header } from 'semantic-ui-react';

import MenuLayout from 'components/layout/MenuLayout';
import Title from 'components/misc/Title';

import 'codemirror/lib/codemirror.css';
import 'syntaxHighlighting/codemirror-highlighting';
import 'components/playground/PlaygroundView.scss';
import Heading from 'components/home/Heading';

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

const codeMirrorOptions = {
  lineNumbers: true,
  mode: 'tlang',
  theme: 'tlang',
  undoDepth: 200,
  indentWithTabs: true,
  tabSize: 4,
  indentUnit: 4,
  showCursorWhenSelecting: true,
};

export default class PlaygroundView extends React.Component<{}, {}> {

  Content = () => {
    return (
      <Segment style={{ border: 'none' }}>
        <Title>Playground</Title>
        Here you can try out <code>tlang</code> in real time. Enter code below and press the Evaluate button
        to have it evaluated (or press <code>CMD</code> + <code>ENTER</code>).

        If you're unsure what to write you can start out with some examples by selecting one in the menu
        to the left.
        <Divider />
        <Grid>
          <Grid.Column width={10}>
            <Header as="h1">Editor</Header>
            <ReactCodeMirror className="CodeWindow" value={codeExample} options={codeMirrorOptions}/>
            <Button icon labelPosition="right" secondary size="large">
              Evaluate <Icon name="check"/>
            </Button>
          </Grid.Column>
          <Grid.Column width={6} stretched>
            <Header as="h1">Results</Header>
            <Segment>LOLOL</Segment>
          </Grid.Column>
        </Grid>
      </Segment>
    );
  }

  render() {
    return <MenuLayout menu={() => <div/>} content={this.Content}/>;
  }
}

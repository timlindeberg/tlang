import * as React from 'react';

import { Controlled as CodeMirror } from 'react-codemirror2';
import { Accordion, Menu, Segment, Divider, Button, Icon, Grid, Header } from 'semantic-ui-react';
import { findFilesWithNames } from 'utils/misc';

import MenuLayout from 'components/layout/MenuLayout';
import Title from 'components/misc/Title';

import 'codemirror/lib/codemirror.css';
import 'syntaxHighlighting/codemirror-highlighting';
import 'components/playground/PlaygroundView.less';

const codeExamples = findFilesWithNames(require.context('codeExamples', true, /\.t/));

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

interface PlaygroundViewState {
  code: string;
}

export default class PlaygroundView extends React.Component<{}, {}> {

  state: PlaygroundViewState = { code:  codeExamples['Hello World'] };

  updateCode = (code: string) => this.setState({ code });

  Menu = () => {
    return (
      <Accordion
        as={Menu}
        inverted
        borderless
        fluid
        vertical
        size="small"
      >
        <Menu.Item>
          <Accordion.Title active content="Examples"/>
          <Menu.Menu>
            {Object
              .keys(codeExamples)
              .sort((a, b) => a.localeCompare(b, undefined, { numeric: true, sensitivity: 'base' }))
              .map(key => (
              <Menu.Item onClick={() => this.updateCode(codeExamples[key])}>{key}</Menu.Item>
            ))}
          </Menu.Menu>
        </Menu.Item>
      </Accordion>
    );
  }

  Content = () => {
    return (
      <Segment className="content-segment">
        <Title>Playground</Title>
        Here you can try out <code>tlang</code> in real time. Enter code below and press the Evaluate button
        to have it evaluated (or press <code>CMD</code> + <code>ENTER</code>).

        If you're unsure what to write you can start out with some examples by selecting one in the menu
        to the left.
        <Divider />
        <Grid>
          <Grid.Column width={10}>
            <Header as="h1">Editor</Header>
            <CodeMirror
              className="CodeWindow shadow-hover"
              value={this.state.code}
              options={codeMirrorOptions}
              onBeforeChange={(editor, data, value) => this.updateCode(value)}
            />
            <Button icon labelPosition="right" secondary size="large">
              Run Code! <Icon name="cogs"/>
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
    console.log('this.state', this.state);
    return <MenuLayout menu={this.Menu} content={this.Content}/>;
  }
}

import * as React from 'react';
import * as API from 'api';

import { Controlled as CodeMirror } from 'react-codemirror2';
import { Segment, Button, Icon, Grid, Header } from 'semantic-ui-react';
import { findFilesWithNames } from 'utils/misc';

import MenuLayout from 'components/layout/MenuLayout';
import PlaygroundMenu from 'components/playground/PlaygroundMenu';

import 'codemirror/lib/codemirror.css';
import 'syntaxHighlighting/codemirror-highlighting';
import 'components/playground/PlaygroundView.less';

interface PlaygroundViewState {
  code: string;
  ws?: WebSocket;
  result?: string[];
}

enum MessageType {
  EVALUATE,
  CANCEL,
  TIMEOUT,
  SUCCESS,
  COMPILATION_ERROR,
  EXECUTION_ERROR,
  INTERNAL_COMPILER_ERROR,
}

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

export default class PlaygroundView extends React.Component<{}, {}> {

  state: PlaygroundViewState = { code: codeExamples['Hello World'] };

  updateCode = (code: string) => this.setState({ code });

  compileCode = () => this.sendMessage(MessageType.EVALUATE, { code: this.state.code });
  cancelCompilation = () => this.sendMessage(MessageType.CANCEL);

  onMessageReceived = (event: any) => {
    console.log('event.data', event.data);
    const message = JSON.parse(event.data);

    const messageKey = message.messageType as keyof typeof MessageType

    switch (MessageType[messageKey]) {
      case MessageType.SUCCESS:
        this.setState({ result: message.result.split("\n") });
        break;
      case MessageType.COMPILATION_ERROR:
        this.setState({ });
        break;

    }
  }

  sendMessage = (messageType: MessageType, data?: any) => {
    const message = JSON.stringify({ messageType: MessageType[messageType], ...data });
    console.log('message', message);
    this.state.ws!.send(message);
  }

  async componentDidMount() {
    const ws = await API.connectToCompilationService();
    ws.onmessage = this.onMessageReceived;
    console.log("Connected");
    this.setState({ ws });
  }

  componentWillUnmount() {
    const ws = this.state.ws;
    if (ws) {
      ws.close();
    }
  }

  Content = () => {
    const result = this.state.result;
    return (
      <Segment className="content-segment">
        <Grid>
          <Grid.Column width={10}>
            <CodeMirror
              className="CodeWindow shadow-hover"
              value={this.state.code}
              options={codeMirrorOptions}
              onBeforeChange={(editor, data, value) => this.updateCode(value)}
            />
            <Button icon labelPosition="right" secondary size="large" onClick={this.compileCode}>
              Run Code! <Icon name="cogs"/>
            </Button>
          </Grid.Column>
          <Grid.Column width={6}>
            <Segment>
              <Header as="h1">Results</Header>
              <div className="result-segment">
                {result && result.map(line => <React.Fragment key={line}>{line}<br/></React.Fragment>)}
              </div>
            </Segment>
          </Grid.Column>
        </Grid>
      </Segment>
    );
  }

  Menu = () => <PlaygroundMenu codeExamples={codeExamples} updateCode={this.updateCode}/>;

  render() {
    return <MenuLayout menu={this.Menu} content={this.Content}/>;
  }
}

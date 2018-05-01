import * as API from 'api';
import * as codemirror from 'codemirror';
import * as React from 'react';

import { TextMarker } from 'codemirror';
import {
  CanceledEvent,
  CompilationErrorEvent,
  CompilationSuccessfulEvent,
  ConnectedEvent,
  ConnectedFailedEvent,
  DisconnectedEvent,
  PlaygroundEvent,
} from 'components/playground/PlaygroundEvents';
import { CompilationError, toCodeMirrorPosition } from 'components/playground/PlaygroundTypes';
import { Controlled as CodeMirror, IInstance } from 'react-codemirror2';
import { Grid, Icon, Segment } from 'semantic-ui-react';
import { findFilesWithNames } from 'utils/misc';

import Navbar from 'components/layout/Navbar';
import Logo from 'components/misc/Logo';
import EventLog from 'components/playground/EventLog';
import PlaygroundMenu from 'components/playground/PlaygroundMenu';

import 'codemirror/lib/codemirror.css';
import 'components/playground/PlaygroundView.less';
import 'syntaxHighlighting/codemirror-highlighting';

interface PlaygroundViewState {
  code: string;
  ws?: WebSocket;
  events: PlaygroundEvent[];
  playgroundState: PlaygroundState;

}

function sleep(ms: number) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

export enum PlaygroundState {
  Disconnected,
  Connecting,
  Waiting,
  Compiling,
}

export enum MessageType {
  EVALUATE,
  CANCEL,
  TIMEOUT,
  SUCCESS,
  COMPILATION_ERROR,
  EXECUTION_ERROR,
  INTERNAL_COMPILER_ERROR,
}

const codeExamples = findFilesWithNames(require.context('codeExamples', true, /\.t/));
const codeMirrorOptions: codemirror.EditorConfiguration = {
  lineNumbers: true,
  mode: 'tlang',
  theme: 'tlang',
  undoDepth: 200,
  indentWithTabs: true,
  tabSize: 4,
  indentUnit: 4,
  showCursorWhenSelecting: true,
  gutters: ['errors', 'CodeMirror-linenumbers'],
};

export default class PlaygroundView extends React.Component<{}, {}> {

  state: PlaygroundViewState = {
    code: codeExamples['Hello World'],
    playgroundState: PlaygroundState.Disconnected,
    events: [],
  };

  editor?: IInstance;
  marks: TextMarker[] = [];

  async componentDidMount() {
    await this.connect();
  }

  componentWillUnmount() {
    const ws = this.state.ws;
    if (ws) {
      ws.close();
    }
  }

  setCode = (code: string) => {
    this.editor!.clearGutter('errors');
    this.marks.forEach(_ => _.clear());
    this.marks = [];
    this.setState({ code });
  }

  compileCode = () => {
    this.sendMessage(MessageType.EVALUATE, { code: this.state.code });
    this.goTo(PlaygroundState.Compiling);
  }

  cancelCompilation = () => {
    this.sendMessage(MessageType.CANCEL);
    this.goTo(PlaygroundState.Waiting);
  }

  onMessageReceived = async (msg: any) => {
    // await sleep(1000);

    const message = JSON.parse(msg.data);
    console.log('Message', message);

    const messageKey = message.messageType as keyof typeof MessageType;
    switch (MessageType[messageKey]) {
    case MessageType.SUCCESS:
      this.addEvent(new CompilationSuccessfulEvent(message));
      break;
    case MessageType.COMPILATION_ERROR:
      this.onCompilationError(message);
      break;
    case MessageType.CANCEL:
      this.addEvent(new CanceledEvent());
      break;
    default:
      this.addEvent({ title: 'Unknown messagetype', icon: 'military', body: () => null, color: 'red' });
    }

    this.goTo(PlaygroundState.Waiting);
  }

  onCompilationError = (message: any) => {
    this.addEvent(new CompilationErrorEvent(message));

    const errorMarker = <Icon name="exclamation triangle"/>;
    const e = document.createElement('i');

    message.errors.forEach((error: CompilationError) => {
      const start = toCodeMirrorPosition(error.start);
      const end = toCodeMirrorPosition(error.end);

      this.editor!.setGutterMarker(start.line, 'errors', e);

      const mark = this.editor!.markText(start, end, { className: 'error-marked' });
      this.marks.push(mark);
    });
  }

  goTo = (playgroundState: PlaygroundState) => this.setState(() => ({ playgroundState }));

  addEvent = (event: PlaygroundEvent): void => {
    this.setState(({ events }: PlaygroundViewState) => ({ events: [...events, event] }));
  }

  sendMessage = (messageType: MessageType, data?: any) => {
    const message = JSON.stringify({ messageType: MessageType[messageType], ...data });
    this.state.ws!.send(message);
  }

  clearEvents = () => this.setState(() => ({ events: [] }));

  connect = async () => {
    this.goTo(PlaygroundState.Connecting);

    const setDisconnected = (event: PlaygroundEvent) => {
      this.goTo(PlaygroundState.Disconnected);
      this.addEvent(event);
    };

    let ws: WebSocket;
    try {
      ws = await API.connectToCompilationService();
    } catch (e) {
      setDisconnected(new ConnectedFailedEvent());
      return;
    }

    ws.onmessage = this.onMessageReceived;
    ws.onclose = () => setDisconnected(new DisconnectedEvent());
    ws.onerror = ws.onclose;

    await sleep(1000);

    this.setState(() => ({ ws }));
    this.goTo(PlaygroundState.Waiting);
    this.addEvent(new ConnectedEvent());
  }

  render() {
    const { code, events } = this.state;
    return (
      <React.Fragment>
        <Segment textAlign="left" inverted id="MenuLayout-navbar" className="Navbar-border">
          <Grid >
            <Grid.Column>
              <Logo size={2.5}/>
            </Grid.Column>
            <Grid.Column>
              <Navbar/>
            </Grid.Column>
          </Grid>
        </Segment>
        <div className="PlaygroundView-content">
          <PlaygroundMenu
            codeExamples={codeExamples}
            setCode={this.setCode}
            compileCode={this.compileCode}
            playgroundState={this.state.playgroundState}
            connect={this.connect}
            clearEvents={this.clearEvents}
            cancelCompilation={this.cancelCompilation}
          />
          <Grid>
            <Grid.Column width={10} className="no-padding-bottom">
              <CodeMirror
                className="CodeWindow shadow-hover"
                value={code}
                options={codeMirrorOptions}
                onBeforeChange={(editor, data, value) => this.setCode(value)}
                editorDidMount={editor => this.editor = editor}
              />
            </Grid.Column>
            <Grid.Column width={6} className="PlaygroundView-result-column no-padding-bottom">
              <EventLog events={events}/>
            </Grid.Column>
          </Grid>
        </div>
      </React.Fragment>
    );
  }
}

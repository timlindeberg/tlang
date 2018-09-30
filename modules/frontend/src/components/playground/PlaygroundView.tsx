import * as API from 'api';
import * as React from 'react';

import {
  CanceledEvent,
  CompilationErrorEvent,
  CompilationSuccessfulEvent,
  ConnectedEvent,
  ConnectedFailedEvent,
  DisconnectedEvent, ExecutionError, InternalCompilerError, NoOutputEvent,
  PlaygroundEvent, ServerError, TimeoutEvent,
} from 'components/playground/events/Events';
import { CodeError } from 'components/playground/PlaygroundTypes';
import Grid from 'semantic-ui-react/dist/commonjs/collections/Grid/Grid';
import Segment from 'semantic-ui-react/dist/commonjs/elements/Segment/Segment';

import Navbar from 'components/layout/Navbar';
import Logo from 'components/misc/Logo';
import EventLog from 'components/playground/EventLog';
import PlaygroundMenu from 'components/playground/PlaygroundMenu';

import 'codemirror/lib/codemirror.css';
import CodeEditor from 'components/playground/CodeEditor';
import codeExamples from 'components/playground/codeExamples';
import 'components/playground/PlaygroundView.less';
import Footer from 'components/layout/Footer';
import 'syntaxHighlighting/codemirror-highlighting';

interface PlaygroundViewState {
  code: string;
  ws?: WebSocket;
  errors: CodeError[];
  events: PlaygroundEvent[];
  playgroundState: PlaygroundState;

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
  NO_OUTPUT,
  COMPILATION_ERROR,
  EXECUTION_ERROR,
  INTERNAL_COMPILER_ERROR,
  SERVER_ERROR,
}

export default class PlaygroundView extends React.Component<{}, {}> {

  state: PlaygroundViewState = {
    code: codeExamples['Hello World'],
    playgroundState: PlaygroundState.Disconnected,
    errors: [],
    events: [],
  };

  menuFunctions = () => ({
    setCode: this.setCode,
    compileCode: this.compileCode,
    connect: this.connect,
    clearEvents: this.clearEvents,
    cancelCompilation: this.cancelCompilation,
  })

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
    this.setState(() => ({ code }));
    if (this.state.errors.length > 0) {
      this.setState(() => ({ errors: [] }));
    }
  }

  compileCode = () => {
    if (this.state.playgroundState !== PlaygroundState.Waiting) {
      return;
    }

    this.sendMessage(MessageType.EVALUATE, { code: this.state.code });
    this.goTo(PlaygroundState.Compiling);
  }

  cancelCompilation = () => {
    this.sendMessage(MessageType.CANCEL);
    this.goTo(PlaygroundState.Waiting);
  }

  onMessageReceived = async (msg: any) => {
    const message = JSON.parse(msg.data);
    const messageKey = message.messageType as keyof typeof MessageType;
    switch (MessageType[messageKey]) {
    case MessageType.SUCCESS:
      this.addEvent(new CompilationSuccessfulEvent(message));
      break;
    case MessageType.NO_OUTPUT:
      this.addEvent(new NoOutputEvent());
      break;
    case MessageType.COMPILATION_ERROR:
      this.addEvent(new CompilationErrorEvent(message));
      this.setState(() => ({ errors: message.errors }));
      break;
    case MessageType.EXECUTION_ERROR:
      this.addEvent(new ExecutionError(message));
      const error: CodeError = { message: message.error };
      const line = message.line;
      if (line !== -1) {
        error.start = { line, col: 1 };
      }
      console.log('line', line);
      console.log('error', error);
      this.setState(() => ({ errors: [error] }));
      break;
    case MessageType.INTERNAL_COMPILER_ERROR:
      this.addEvent(new InternalCompilerError(message));
      break;
    case MessageType.CANCEL:
      this.addEvent(new CanceledEvent());
      break;
    case MessageType.TIMEOUT:
      this.addEvent(new TimeoutEvent(message));
      break;
    case MessageType.SERVER_ERROR:
      this.addEvent(new ServerError());
      break;
    default:
      this.addEvent({ title: 'Unknown messagetype', icon: 'military', body: () => null, color: 'red' });
    }

    this.goTo(PlaygroundState.Waiting);
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
    ws.onerror = ws.onclose = () => setDisconnected(new DisconnectedEvent());

    this.setState(() => ({ ws }));
    this.goTo(PlaygroundState.Waiting);
    this.addEvent(new ConnectedEvent());
  }

  render() {
    const { code, events, errors, playgroundState } = this.state;
    return (
      <React.Fragment>
        <Segment textAlign="left" inverted id="MenuLayout-navbar" className="Navbar-border">
          <Grid>
            <Grid.Column>
              <Logo size={2.5}/>
            </Grid.Column>
            <Grid.Column>
              <Navbar/>
            </Grid.Column>
          </Grid>
        </Segment>
        <div id="PlaygroundView-content">
          <PlaygroundMenu playgroundState={playgroundState} {...this.menuFunctions()}/>
          <Grid>
            <Grid.Column width={10} className="no-padding-bottom">
              <CodeEditor code={code} setCode={this.setCode} compileCode={this.compileCode} errors={errors}/>
            </Grid.Column>
            <Grid.Column width={6} className="PlaygroundView-result-column no-padding-bottom">
              <EventLog events={events}/>
            </Grid.Column>
          </Grid>
        </div>
        <Footer />
      </React.Fragment>
    );
  }
}

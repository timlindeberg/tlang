import codeExamples from 'components/playground/codeExamples';
import * as React from 'react';
import { Dropdown, Icon, Loader, Menu } from 'semantic-ui-react';
import HelpModal from './HelpModal';
import { PlaygroundState } from './PlaygroundView';

interface PlaygroundMenuProps {
  setCode: (s: string) => void;
  compileCode: () => void;
  connect: () => void;
  playgroundState: PlaygroundState;
  clearEvents: () => void;
  cancelCompilation: () => void;
}

export default class PlaygroundMenu extends React.Component<PlaygroundMenuProps, {}> {

  connectionStatusItem = () => {
    const { playgroundState, connect } = this.props;
    const className = `PlaygroundState ${PlaygroundState[playgroundState]}`;
    switch (playgroundState) {
    case PlaygroundState.Waiting:
    case PlaygroundState.Compiling:
      return (
          <Menu.Item className={className}>
            <span>Connected</span>
          </Menu.Item>
      );
    case PlaygroundState.Connecting:
      return (
          <Menu.Item className={className}>
            <span>Connecting...</span>
            <Loader inline size="mini" active />
          </Menu.Item>
      );
    case PlaygroundState.Disconnected:
      return (
          <Menu.Item className={className} onClick={connect}>
            <Icon name="refresh"/>
            <span>Disconnected</span>
          </Menu.Item>
      );
    default: throw new Error(`Unhandled playground state: ${playgroundState}`);

    }
  }

  runCodeItem = () => {
    const { compileCode, playgroundState, cancelCompilation } = this.props;

    const className = `CompileButton ${PlaygroundState[playgroundState]}`;
    if (playgroundState === PlaygroundState.Compiling) {
      return (
        <Menu.Item onClick={cancelCompilation} className={className}>
          <Icon name="cancel"/>
          Cancel
        </Menu.Item>
      );
    }

    const isConnected = playgroundState === PlaygroundState.Waiting;
    return (
      <Menu.Item onClick={compileCode} disabled={!isConnected} className={className}>
        <Icon name="cogs"/>
        Run Code
      </Menu.Item>
    );
  }

  examplesItem = () => {
    const { setCode } = this.props;
    return (
      <Dropdown item text="Examples">
        <Dropdown.Menu>
          {Object.keys(codeExamples)
            .sort((a, b) => a.localeCompare(b, undefined, { numeric: true, sensitivity: 'base' }))
            .map(key => (
              <Dropdown.Item key={key} onClick={() => setCode(codeExamples[key])}>
                {key}
              </Dropdown.Item>
            ))}
        </Dropdown.Menu>
      </Dropdown>
    );
  }

  clearEventLogItem = () => {
    return (
      <Menu.Item onClick={this.props.clearEvents} className="ClearEventLogButton">
        <Icon name="x"/>
        Clear Event Log
      </Menu.Item>
    );
  }

  render() {
    return (
      <Menu id="PlaygroundView-menu" className="shadow-hover" widths={5}>
        {this.connectionStatusItem()}
        {this.runCodeItem()}
        <HelpModal/>
        {this.examplesItem()}
        {this.clearEventLogItem()}
      </Menu>
    );
  }

}

import 'components/playground/Events.less';
import { CodeError } from 'components/playground/PlaygroundTypes';
import * as React from 'react';
import { SemanticCOLORS, SemanticICONS, Accordion, Divider } from 'semantic-ui-react';
import { htmlLines, thousandSeperatedNumber, widthOfRenderedText } from 'utils/misc';
import { Collapse } from 'react-collapse';
import { HashLink } from "react-router-hash-link";

export abstract class PlaygroundEvent {
  abstract title: string;
  abstract color: SemanticCOLORS;
  abstract icon: SemanticICONS;

  body(): any {
    return null;
  }
}

const LINES_TO_SHOW: number = 15;


interface CompilationSuccessfulBodyProps {
  lines: string[];
}
interface CompilationSuccessfulState {
  isOpen: boolean;
}


class CompilationSuccessfulBody extends React.Component<CompilationSuccessfulBodyProps, CompilationSuccessfulState> {
  textWidth: number = 0;
  state: CompilationSuccessfulState = {
    isOpen: false
  };

  constructor(props: CompilationSuccessfulBodyProps) {
    super(props);
    this.textWidth = widthOfRenderedText(`${props.lines.length}`, 'line-number');
  }

  toggleOpen = () => this.setState((state) => ({ isOpen: !state.isOpen }));
  
  makeLine = (line: string, i: number): JSX.Element => (
    <React.Fragment key={i}>
      <span className="line-number" style={{ width: `${this.textWidth}px` }}>{i + 1}</span>
      <span className="result-line">{line}</span>
      <br/>
    </React.Fragment>
  );

  getToggleText = (isOpen: Boolean, length: number): string =>
    isOpen ? 'Show less' : `Show ${thousandSeperatedNumber(length)} more line${length > 1 ? 's' : ''}`

  render() {
    const { lines } = this.props;
    const { isOpen } = this.state;
    
    const first = lines.slice(0, LINES_TO_SHOW);
    const extra = lines.slice(LINES_TO_SHOW);

    return (
      <div className="result-block">
        {first.map(this.makeLine)}
        {extra.length > 0 && (
          <Accordion>
            <Collapse isOpened={isOpen}>
              <React.Fragment>
                {isOpen && extra.map((line, i) => this.makeLine(line, LINES_TO_SHOW + i))}
              </React.Fragment>
            </Collapse>
            <Divider />
            <Accordion.Title
              active={isOpen}
              content={this.getToggleText(isOpen, extra.length)}
              onClick={this.toggleOpen}
              className="show-more"
            />
          </Accordion>
        )}
      </div>
    );
  }
}

export class CompilationSuccessfulEvent extends PlaygroundEvent {
  title: string = 'Result';
  color: SemanticCOLORS = 'green';
  icon: SemanticICONS = 'check';
  private lines: string[];

  constructor(message: any) {
    super();
    this.lines = message.result.split('\n');
  }

  body() {
    return <CompilationSuccessfulBody lines={this.lines}/>
  }
}

export class NoOutputEvent extends PlaygroundEvent {
  title: string = 'No output';
  color: SemanticCOLORS = 'green';
  icon: SemanticICONS = 'circle thin';

  body() {
    return 'Compilation was successful but there was no output. Use print or println to output the results of your program.';
  }
}

export class CompilationErrorEvent extends PlaygroundEvent {
  title: string = 'Compilation error';
  color: SemanticCOLORS = 'red';
  icon: SemanticICONS = 'exclamation triangle';

  private readonly errors: CodeError[];
  private readonly positions: string[];
  private readonly widthOfLongestPosition: number;

  constructor(message: any) {
    super();
    this.errors = message.errors;
    this.positions = this.errors.map(e => e.start ? `${e.start.line}:${e.start.col}` : '');

    const longest = this.positions.slice().sort((a, b) => b.length - a.length)[0];
    this.widthOfLongestPosition = widthOfRenderedText(longest, 'line-number');
  }

  body() {
    const width = `${this.widthOfLongestPosition}px`;
    return (
      <div className="result-block">
        {this.errors.map((error, i) =>
          <React.Fragment key={i}>
            <span className="line-number" style={{ width }}>{this.positions[i]}</span>
            <span className="result-line">{error.message}</span>
            <br/>
          </React.Fragment>
        )}
      </div>
    );
  }
}

export class ExecutionError extends PlaygroundEvent {
  title: string = 'Execution error';
  color: SemanticCOLORS = 'red';
  icon: SemanticICONS = 'exclamation circle';
  private lines: string[];

  constructor(message: any) {
    super();
    this.lines = message.error.split('\n');
  }

  body() {
    return (
      <div className="result-block">
        <p>Execution exited with an exception:</p>
        <p className="result-block stacktrace">{htmlLines(this.lines, 'error-line')}</p>
      </div>
    );
  }
}

export class InternalCompilerError extends PlaygroundEvent {
  title: string = 'Internal compiler error';
  color: SemanticCOLORS = 'red';
  icon: SemanticICONS = 'warning';

  private readonly error: string;

  constructor(message: any) {
    super();
    this.error = message.error;
  }

  body() {
    return (
      <div className="result-block">
        <p>The compiler crashed with an internal error:</p>
        <p className="result-block stacktrace">{this.error}</p>
      </div>
    );
  }
}

export class ConnectedEvent extends PlaygroundEvent {
  title: string = 'Connected to server';
  color: SemanticCOLORS = 'green';
  icon: SemanticICONS = 'wifi';
}

export class ConnectedFailedEvent extends PlaygroundEvent {
  title: string = 'Failed to connect to server';
  color: SemanticCOLORS = 'red';
  icon: SemanticICONS = 'ban';

  body() {
    return 'Press the reconnect button to try again.';
  }
}

export class DisconnectedEvent extends PlaygroundEvent {
  title: string = 'Disconnected from server';
  color: SemanticCOLORS = 'orange';
  icon: SemanticICONS = 'remove circle';

  body() {
    return 'Press the reconnect button to reconnect.';
  }
}

export class CanceledEvent extends PlaygroundEvent {
  title: string = 'Compilation canceled';
  color: SemanticCOLORS = 'red';
  icon: SemanticICONS = 'remove';
}

export class TimeoutEvent extends PlaygroundEvent {
  title: string = 'Execution timed out';
  color: SemanticCOLORS = 'red';
  icon: SemanticICONS = 'hourglass half';
  private readonly timeout: number;

  constructor(message: any) {
    super();
    this.timeout = message.timeout;
  }

  body() {
    return `Execution timed out after ${this.timeout}s`;
  }
}

export class ServerError extends PlaygroundEvent {
  title: string = 'Internal server error';
  color: SemanticCOLORS = 'red';
  icon: SemanticICONS = 'server';
}

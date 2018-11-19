import BodyWithExtraContent  from 'components/playground/events/BodyWithExtraContent';
import 'components/playground/events/Events.less';
import { CodeError } from 'components/playground/PlaygroundTypes';
import * as React from 'react';
import { HashLink } from 'react-router-hash-link';
import { SemanticCOLORS, SemanticICONS } from 'semantic-ui-react';
import { htmlLines, widthOfRenderedText } from 'utils/misc';

export abstract class PlaygroundEvent {
  abstract title: string;
  abstract color: SemanticCOLORS;
  abstract icon: SemanticICONS;

  body(maxToShow: number): any {
    return null;
  }
}

export class CompilationSuccessfulEvent extends PlaygroundEvent {
  title: string = 'Result';
  color: SemanticCOLORS = 'green';
  icon: SemanticICONS = 'check';
  textWidth: number;
  private readonly lines: string[];

  constructor(message: any) {
    super();
    this.lines = message.result.split('\n');
    this.textWidth = widthOfRenderedText(`${this.lines.length}`, 'line-number');
  }

  makeLine = (i: number): JSX.Element => (
    <React.Fragment key={i}>
      <span className="line-number" style={{ width: `${this.textWidth}px` }}>{i + 1}</span>
      <span className="result-line">{this.lines[i]}</span>
      <br/>
    </React.Fragment>
  )

  body(maxToShow: number) {
    return (
      <BodyWithExtraContent
        numLines={this.lines.length}
        makeLine={this.makeLine}
        maxToShow={maxToShow}
        type="line"
        width={this.textWidth}
      />
    );
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

  makeLine = (i: number): JSX.Element => (
    <React.Fragment key={i}>
      <span className="line-number" style={{ width: `${this.widthOfLongestPosition}px` }}>{this.positions[i]}</span>
      <span className="result-line">{this.errors[i].message}</span>
      <br/>
    </React.Fragment>
  )

  body(maxToShow: number) {
    return (
      <BodyWithExtraContent
        numLines={this.errors.length}
        makeLine={this.makeLine}
        maxToShow={maxToShow}
        type="error"
        width={this.widthOfLongestPosition}
      />
    );
  }
}

export class NoOutputEvent extends PlaygroundEvent {
  title: string = 'No output';
  color: SemanticCOLORS = 'green';
  icon: SemanticICONS = 'circle thin';

  body(maxToShow: number) {
    return (
      <p>
        Compilation was successful but there was no output.
        Use <code>print</code> or <code>println</code> to output the results of your program.
      </p>
    );
  }
}

export class ExecutionError extends PlaygroundEvent {
  title: string = 'Execution error';
  color: SemanticCOLORS = 'red';
  icon: SemanticICONS = 'exclamation circle';
  private readonly lines: string[];

  constructor(message: any) {
    super();
    this.lines = message.error.split('\n');
  }

  body(maxToShow: number) {
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

  body(maxToShow: number) {
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

  body(maxToShow: number) {
    return 'Press the reconnect button to try again.';
  }
}

export class DisconnectedEvent extends PlaygroundEvent {
  title: string = 'Disconnected from server';
  color: SemanticCOLORS = 'orange';
  icon: SemanticICONS = 'remove circle';

  body(maxToShow: number) {
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

  body(maxToShow: number) {
    return `Execution timed out after ${this.timeout}s`;
  }
}

export class ServerError extends PlaygroundEvent {
  title: string = 'Internal server error';
  color: SemanticCOLORS = 'red';
  icon: SemanticICONS = 'server';
}

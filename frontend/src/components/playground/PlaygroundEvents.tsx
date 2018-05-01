import 'components/playground/PlaygroundEvents.less';
import { CompilationError } from 'components/playground/PlaygroundTypes';
import * as React from 'react';
import { SemanticCOLORS, SemanticICONS } from 'semantic-ui-react';
import { widthOfRenderedText } from 'utils/misc';

export abstract class PlaygroundEvent {
  abstract title: string;
  abstract color: SemanticCOLORS;
  abstract icon: SemanticICONS;

  body(): any {
    return null;
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
    const textWidth = widthOfRenderedText(`${this.lines.length}`, 'line-number');
    return (
      <div className="result-block">
        {this.lines.map((line, i) =>
          <React.Fragment key={i}>
            <span className="line-number" style={{ width: `${textWidth}px` }}>{i + 1}</span>
            <span className="result-line">{line}</span>
            <br/>
          </React.Fragment>
        )}
      </div>
    );
  }
}

export class CompilationErrorEvent extends PlaygroundEvent {
  title: string = 'Compilation error';
  color: SemanticCOLORS = 'red';
  icon: SemanticICONS = 'exclamation triangle';

  private readonly errors: CompilationError[];
  private readonly positions: string[];
  private readonly widthOfLongestPosition: number;

  constructor(message: any) {
    super();
    this.errors = message.errors;
    this.positions = this.errors.map(({ start }) => `${start.line}:${start.col}`);

    const longest = this.positions.sort((a, b) => b.length - a.length)[0];
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

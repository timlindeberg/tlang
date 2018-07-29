import * as codemirror from 'codemirror';
import { TextMarker } from 'codemirror';
import { CodeError, toCodeMirrorPosition } from 'components/playground/PlaygroundTypes';
import * as _ from 'lodash';
import * as React from 'react';
import { Controlled as CodeMirror, IInstance } from 'react-codemirror2';
import { Icon, Popup } from 'semantic-ui-react';
import { asDOM, htmlLines } from 'utils/misc';

const CODE_MIRROR_OPTIONS: codemirror.EditorConfiguration = {
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

interface CodeEditorProps {
  code: string;
  setCode: (code: string) => void;
  compileCode: () => void;
  errors: CodeError[];
}

interface CodeEditorState {
}

export default class CodeEditor extends React.Component<CodeEditorProps, CodeEditorState> {
  state: CodeEditorState = {};
  editor?: IInstance;
  marks: TextMarker[] = [];

  setCode = (editor: any, data: any, value: string) => this.props.setCode(value);

  onKeyPress = (editor: IInstance, event: KeyboardEvent) => {
    if (event.ctrlKey && event.code === 'Enter') {
      this.props.compileCode();
    }
  }

  componentDidUpdate(prevProps: CodeEditorProps) {
    if (prevProps.errors === this.props.errors) {
      return;
    }

    const errors = this.props.errors;

    this.editor!.clearGutter('errors');
    this.marks.forEach(m => m.clear());

    errors.forEach(this.markErrorInText);
    const grouped = _.groupBy(errors, error => error.start ? error.start.line : -1);
    Object.keys(grouped).forEach(line => this.addErrorGutter(grouped[line]));
  }

  markErrorInText = (error: CodeError) => {
    if (!error.start || !error.end) {
      return;
    }

    const start = toCodeMirrorPosition(error.start);
    const end = toCodeMirrorPosition(error.end);

    const mark = this.editor!.markText(start, end, { className: 'error-marked flash animated' });
    this.marks.push(mark);
  }

  addErrorGutter = (errors: CodeError[]) => {
    const first = errors[0];

    if (!first.start) {
      return;
    }

    const icon = <Icon name="exclamation circle" color="red" className="bounceIn animated"/>;
    const errorMarker = asDOM(
      <Popup
        wide
        className="error-popup fadeIn animated"
        position="right center"
        trigger={icon}
      >
        {errors.map((e, i) => <div key={i}>{htmlLines(e.message, 'errorLine')}</div>)}
      </Popup>
    );

    const line = first.start.line - 1;
    this.editor!.setGutterMarker(line, 'errors', errorMarker);
  }

  render() {
    return (
      <CodeMirror
        className="CodeWindow shadow-hover"
        value={this.props.code}
        options={CODE_MIRROR_OPTIONS}
        onBeforeChange={this.setCode}
        editorDidMount={editor => this.editor = editor}
        onKeyPress={this.onKeyPress}
      />
    );
  }

}

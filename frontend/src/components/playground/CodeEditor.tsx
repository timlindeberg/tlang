import * as codemirror from 'codemirror';
import { TextMarker } from 'codemirror';
import { CompilationError, toCodeMirrorPosition } from 'components/playground/PlaygroundTypes';
import * as _ from 'lodash';
import * as React from 'react';
import { Controlled as CodeMirror, IInstance } from 'react-codemirror2';
import { Icon, Popup } from 'semantic-ui-react';
import { asDOM } from 'utils/misc';

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
  errors: CompilationError[];
}

interface CodeEditorState {}

export default class CodeEditor extends React.Component<CodeEditorProps, CodeEditorState> {
  state: CodeEditorState = {};
  editor?: IInstance;
  marks: TextMarker[] = [];

  setCode = (editor: any, data: any, value: string) => this.props.setCode(value);

  onKeyPress = (editor: IInstance, event: KeyboardEvent) => {
    console.log(event);
    if ((event.ctrlKey || event.metaKey) && event.code === 'Space') {
      this.props.compileCode();
    }
  }

  componentDidUpdate(prevProps: CodeEditorProps) {
    if (prevProps.errors === this.props.errors) {
      return;
    }

    const errors = this.props.errors;

    this.editor!.clearGutter('errors');
    this.marks.forEach(_ => _.clear());

    errors.forEach(this.markErrorInText);
    const grouped = _.groupBy(errors, error => error.start.line);
    Object.keys(grouped).forEach(line => this.addErrorGutter(grouped[line]));
  }

  markErrorInText = (error: CompilationError) => {
    const start = toCodeMirrorPosition(error.start);
    const end = toCodeMirrorPosition(error.end);
    const editor = this.editor!;

    const text = editor.getRange(start, end);
    const span = <span className="error-marked flash animated">{text}</span>;
    const lineMarker = asDOM(
      <Popup
        wide
        className="error-popup fadeIn animated"
        position="top center"
        trigger={span}
        content={error.message}
      />,
      'span'
    );

    const mark = editor.markText(start, end, { replacedWith: lineMarker });
    this.marks.push(mark);
  }

  addErrorGutter = (errors: CompilationError[]) => {
    const icon = <Icon name="exclamation circle" color="red" className="bounceIn animated"/>;
    const errorMarker = asDOM(
      <Popup
        wide
        className="error-popup fadeIn animated"
        position="right center"
        trigger={icon}
      >
        {errors.map((e, i) => <div key={i}>{e.message}</div>)}
      </Popup>
    );

    const line = errors[0].start.line - 1;
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
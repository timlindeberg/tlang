export interface ErrorPosition {
  line: number;
  col: number;
}

export function toCodeMirrorPosition(pos: ErrorPosition): CodeMirror.Position {
  return { line: pos.line - 1, ch: pos.col - 1 };
}

export interface CodeError {
  start?: ErrorPosition;
  end?: ErrorPosition;
  message: string;
}
export enum Type {
  Root = 'root',
  Text = 'text',
  Paragraph = 'paragraph',
  Heading = 'heading',
  List = 'list',
  ListItem = 'listItem',
  ThematicBreak = 'thematicBreak',
  Link = 'link',
  Emphasis = 'emphasis',
  Strong = 'strong',
  InlineCode = 'inlineCode',
  Image = 'image',
  Table = 'table',
  TableRow = 'tableRow',
  TableCell = 'tableCell',
  TableColumn = 'tableColumn',
  Code = 'code',
  BlockQuote = 'blockquote',
}

export interface AST {
  type: Type;
  children: AST[];
  value?: string;
  depth?: number;
  ordered?: boolean;
  url?: string;
  alt?: string;
  lang?: string;
}

import * as React from 'react';
import * as ReactMarkdown from 'react-markdown';
import CodeBlock from 'components/CodeBlock';
import { Header } from 'semantic-ui-react';

const renderers = {
  heading: (props: any) => <Header as={`h${props.level}`}>{props.children}</Header>,
  code: (props: any) => <CodeBlock language={props.language}>{props.value}</CodeBlock>,
};

interface MarkdownProps {
  children: string;
}

const Markdown: React.StatelessComponent<MarkdownProps> = ({ children }: MarkdownProps) => (
  <ReactMarkdown source={children} renderers={renderers} />
);

export default Markdown;

import * as React from 'react';
import 'components/misc/Title.scss';

interface TitleProps {
  children: string;
}

export default class Title extends React.Component<TitleProps, {}> {

  render() {
    return <div className="Title">{this.props.children}</div>;
  }
}

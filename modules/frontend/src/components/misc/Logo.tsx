import 'components/misc/Logo.less';
import * as React from 'react';
import { Link } from 'react-router-dom';
import Header from 'semantic-ui-react/dist/commonjs/elements/Header/Header';


interface LogoProps {
  size: number;
  link?: boolean;
}

const Logo = ({ size, link = true }: LogoProps) => {
  const header = (
    <Header
      as="h1"
      content="tlang"
      inverted
      id="logo"
      style={{ fontSize: `${size}em` }}
    />
  );
  return link ? <Link to="/">{header}</Link> : header;
};

export default Logo;

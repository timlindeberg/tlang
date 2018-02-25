import * as React from "react";
import { Header } from 'semantic-ui-react';
import 'components/Logo.scss';

interface LogoProps {
  size: number;
}

const Logo = ({ size }: LogoProps) => {
  return (
    <Header
      as="h1"
      content="tlang"
      inverted
      className="logo"
      style={{ fontSize: `${size}em` }}
    />
  )
};

export default Logo;

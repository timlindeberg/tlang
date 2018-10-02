import 'components/layout/Footer.less';
import * as React from 'react';
import Container from 'semantic-ui-react/dist/commonjs/elements/Container/Container';
import Header from 'semantic-ui-react/dist/commonjs/elements/Header/Header';
import Segment from 'semantic-ui-react/dist/commonjs/elements/Segment/Segment';

const bottomStyle = {
  position: 'absolute',
  bottom: 0
};

interface FooterProps {
  bottom: boolean;
}

const Footer = ({ bottom }: FooterProps) => (
  <Segment inverted vertical id="Footer" style={bottom ? bottomStyle : {}} >
    <Container>
      <a href="https://github.com/timlindeberg/tlang">
        <Header as="h4" inverted textAlign="center">https://github.com/timlindeberg/tlang</Header>
      </a>
    </Container>
  </Segment>
);

export default Footer;

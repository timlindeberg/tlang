import * as React from 'react';
import Container from 'semantic-ui-react/dist/commonjs/elements/Container/Container';
import Header from 'semantic-ui-react/dist/commonjs/elements/Header/Header';
import Segment from 'semantic-ui-react/dist/commonjs/elements/Segment/Segment';

const Footer = () => (
  <Segment inverted vertical >
    <Container>
      <a href="https://github.com/timlindeberg/tlang">
        <Header as="h4" inverted textAlign="center">https://github.com/timlindeberg/tlang</Header>
      </a>
    </Container>
  </Segment>
);

export default Footer;

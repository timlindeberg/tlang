import * as React from 'react';
import { Container, Grid, Header, List, Segment } from 'semantic-ui-react';

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

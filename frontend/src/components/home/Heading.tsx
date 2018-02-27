import * as React from 'react';
import { Button, Container, Header, Icon } from 'semantic-ui-react';
import Logo from 'components/Logo';
import 'components/home/Heading.scss';

export interface HomepageHeadingProps {
  mobile: boolean;
}

const Heading = ({ mobile = false }: HomepageHeadingProps) => (
  <Container text style={{ padding: '5em' }}>
    <Logo size={4} />
    <Header
      as="h2"
      content="A supercool language for the JVM"
      inverted
      id="Heading-secondary"
      style={{ paddingTop: '1.5em' }}
    />
    <Button secondary size="huge">
      Try it out!
      <Icon name="arrow right"/>
    </Button>
  </Container>
);

export default Heading;

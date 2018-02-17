import * as React from 'react';
import { Button, Container, Header, Icon } from 'semantic-ui-react';
import 'components/home/Heading.scss';

export interface HomepageHeadingProps {
  mobile: boolean;
}

const Heading = ({ mobile = false }: HomepageHeadingProps) => (
  <Container text>
    <Header
      as="h1"
      content="tlang"
      inverted
      id="Heading-main"
      className={mobile ? 'mobile' : 'desktop'}
    />
    <Header
      as="h2"
      content="A supercool language for the JVM"
      inverted
      id="Heading-secondary"
      className={mobile ? 'mobile' : 'desktop'}
    />
    <Button secondary size="huge">
      Try it out!
      <Icon name="arrow right"/>
    </Button>
  </Container>
);

export default Heading;
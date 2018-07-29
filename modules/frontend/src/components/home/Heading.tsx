import 'Animation.less';
import 'components/home/Heading.less';
import Logo from 'components/misc/Logo';
import * as React from 'react';
import { Link } from 'react-router-dom';
import { Button, Container, Header, Icon } from 'semantic-ui-react';

export interface HomepageHeadingProps {
  mobile: boolean;
}

const Heading = ({ mobile = false }: HomepageHeadingProps) => (
  <Container text style={{ padding: '5em' }} className="animated fade-in-up">
    <Logo link={false} size={4} />
    <Header
      as="h2"
      content="A supercool language for the JVM"
      inverted
      id="Heading-secondary"
      style={{ paddingTop: '1.5em' }}
    />
    <Button icon labelPosition="right" secondary size="huge" as={Link} to="/playground">
      Try it out!
      <Icon name="arrow right"/>
    </Button>
  </Container>
);

export default Heading;

import 'Animation.less';
import 'components/home/Heading.less';
import Logo from 'components/misc/Logo';
import * as React from 'react';
import { Link } from 'react-router-dom';
import Button from 'semantic-ui-react/dist/commonjs/elements/Button/Button';
import Container from 'semantic-ui-react/dist/commonjs/elements/Container/Container';
import Header from 'semantic-ui-react/dist/commonjs/elements/Header/Header';
import Icon from 'semantic-ui-react/dist/commonjs/elements/Icon/Icon';

const Heading = () => (
  <Container text style={{ padding: '5em' }} className="animated fade-in-up">
    <Logo link={false} size={4} />
    <Header
      as="h2"
      content="A useful scripting language for the JVM"
      inverted
      id="Heading-secondary"
      style={{ paddingTop: '1.5em' }}
    />
    <Button icon labelPosition="right" secondary size="large" as={Link} to="/playground">
      Try it out!
      <Icon name="arrow right"/>
    </Button>
  </Container>
);

export default Heading;

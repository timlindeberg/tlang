import 'Animation.less';
import Heading from 'components/home/Heading';
import 'components/home/HomeView.less';
import Footer from 'components/layout/Footer';
import Navbar from 'components/layout/Navbar';
import CodeBlock from 'components/misc/CodeBlock';
import * as React from 'react';
import { LazyImage } from 'react-lazy-images';
import { Link } from 'react-router-dom';
import Grid from 'semantic-ui-react/dist/commonjs/collections/Grid/Grid';
import Button from 'semantic-ui-react/dist/commonjs/elements/Button/Button';
import Container from 'semantic-ui-react/dist/commonjs/elements/Container/Container';
import Divider from 'semantic-ui-react/dist/commonjs/elements/Divider/Divider';
import Header from 'semantic-ui-react/dist/commonjs/elements/Header/Header';
import List from 'semantic-ui-react/dist/commonjs/elements/List/List';
import Segment from 'semantic-ui-react/dist/commonjs/elements/Segment/Segment';

const emptyImage = require('static/images/empty.svg');
const replImage = require('static/images/trepl.svg');
const watchImage = require('static/images/watch.svg');

const codeExample =
`package t::lang

class HelloWorld<T> =

	var times: T

	Def new(times: T?) =  
		this.times = times ?: 1

	Def Print() = 
		for(var i = 0; i < times; i++)
			println("Hello world!")

	Def +(lhs: HelloWorld<T>, rhs: HelloWorld<T>) = 
	new HelloWorld(lhs.times + rhs.times)

// -----------------------------------------------------

val helloWorld = new HelloWorld<Long>(1300L) + 
   new HelloWorld<Long>(37L)

helloWorld.Print() // prints "Hello world!" 1337 times
`;

const TerminalImage = ({ src }: { src: string }) => (
  <LazyImage
    src={src}
    placeholder={({ imageProps, ref }: any) => <img ref={ref} src={emptyImage} className="shadow-hover"/>}
    actual={({ imageProps }: any) => <img {...imageProps} className="shadow-hover"/>}
  />
);

const HomeView = () => (
  <React.Fragment>
    <Segment textAlign="center" vertical inverted id="Heading-segment">
      <Container>
        <Navbar className="animated fade-in-right"/>
      </Container>
      <Heading />
    </Segment>
    <Segment style={{ paddingTop: '4em' }} vertical>
      <Grid container stackable verticalAlign="middle">
        <Grid.Row>
          <Grid.Column width={8} className="animated fade-in-right column-left">
            <Header as="h1">Language philosophy</Header>
            <p className="HomeView-larger-text">
              <code>tlang</code> is a strongly typed scripting language which takes inspiration from
              languages such as Kotlin, python, golang, Java, Scala and C++. Combining object
              oriented design with functional programming ideas together with a clean whitespace
              based syntax.
            </p>
            <Header as="h1">Features</Header>
            <List bulleted className="HomeView-larger-text">
              <List.Item>Object oriented design with classes, traits and inheritance</List.Item>
              <List.Item>C++ like templates with run time generics</List.Item>
              <List.Item>Extension classes</List.Item>
              <List.Item>Fully featured REPL</List.Item>
              <List.Item>Operator overloading</List.Item>
              <List.Item>Integrates easily with existing Java code</List.Item>
              <List.Item>Well designed and easy to read error messages</List.Item>
            </List>
          </Grid.Column>
          <Grid.Column width={8} verticalAlign="middle" className="animated fade-in-left column-right">
            <CodeBlock language="tlang">{codeExample}</CodeBlock>
          </Grid.Column>
        </Grid.Row>
        <Divider />
        <Grid.Row>
          <Grid.Column width={8} className="animated fade-in-right column-left">
            <TerminalImage src={replImage} />
          </Grid.Column>
          <Grid.Column width={8} className="animated fade-in-left column-right">
            <Header as="h1">trepl</Header>
            <List bulleted className="HomeView-larger-text">
              <List.Item>Fully featured REPL</List.Item>
              <List.Item>Evaluate expressions</List.Item>
              <List.Item>Keeps history</List.Item>
              <List.Item>Syntax highlighting</List.Item>
              <List.Item>Supports mouse input and selection</List.Item>
              <List.Item>Easy navigation with ALT and CMD modifiers</List.Item>
              <List.Item>Supports multiline editing</List.Item>
            </List>
          </Grid.Column>
        </Grid.Row>
        <Divider />
        <Grid.Row>
          <Grid.Column width={8} className="animated fade-in-right column-left">
            <Header as="h1">Watch mode</Header>
            <List bulleted className="HomeView-larger-text">
              <List.Item>The t-compiler can watch your files and recompile when it changes</List.Item>
              <List.Item>Combine with the --exec flag to execute and view the output</List.Item>
              <List.Item>Get immediate feedback through easy to read error messages</List.Item>
              <List.Item>Formats the output for easier reading</List.Item>
            </List>
          </Grid.Column>
          <Grid.Column width={8} className="animated fade-in-left column-right">
              <TerminalImage src={watchImage} />
          </Grid.Column>
        </Grid.Row>
        <Divider />
        <Grid.Row>
          <Grid.Column style={{ padding: '4em 0' }} textAlign="center" verticalAlign="middle">
            <Button secondary size="huge" as={Link} to="/getting_started">Get started</Button>
          </Grid.Column>
        </Grid.Row>
      </Grid>
    </Segment>
    <Footer bottom={false}/>
  </React.Fragment>
);

export default HomeView;

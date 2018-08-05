import 'Animation.less';
import DesktopContainer from 'components/home/DesktopContainer';
import MobileContainer from 'components/home/MobileContainer';
import CodeBlock from 'components/misc/CodeBlock';
import Footer from 'Footer';
import * as React from 'react';
import { Button, Grid, Header, List, Segment } from 'semantic-ui-react';
import { LazyImage } from 'react-lazy-images';


interface ResponsiveContainerProps {
  children: any;
}

const codeExample =
`package t::lang

val helloWorld = new HelloWorld<Long>(1300L) + 
   new HelloWorld<Long>(37L)

helloWorld.Print() // prints "Hello world!" 1337 times

class HelloWorld<T> =

	var times: T

	Def new(times: T?) = 
		this.times = times ?: 1

	Def Print() = 
		for(var i = 0; i < times; i++)
			println("Hello world!")

	Def +(lhs: HelloWorld<T>, rhs: HelloWorld<T>) =
		new HelloWorld(lhs.times + rhs.times)`;

const ResponsiveContainer = ({ children }: ResponsiveContainerProps) => (
  <div>
    <DesktopContainer>{children}</DesktopContainer>
    <MobileContainer>{children}</MobileContainer>
  </div>
);

const HomeView = () => (
  <ResponsiveContainer>
    <Segment style={{ paddingTop: '4em' }} vertical>
      <Grid container stackable verticalAlign="middle">
        <Grid.Row>
          <Grid.Column width={8} className="animated fade-in-right">
            <Header as="h1">Language philosophy</Header>
            <p className="HomeView-larger-text">
              tlang takes inspiration from languages such as Kotlin, python, golang, Java, Scala and C++.
              Combining object oriented design with functional programming ideas together with
              a clean whitespace based syntax.
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
          <Grid.Column width={8} verticalAlign="middle" className="animated fade-in-left">
            <CodeBlock language="tlang">{codeExample}</CodeBlock>
          </Grid.Column>
        </Grid.Row>
        <Grid.Row>
          <Grid.Column style={{ padding: '4em 0' }} textAlign="center" verticalAlign="middle">
            <Button secondary size="huge">Get started</Button>
          </Grid.Column>
        </Grid.Row>
        <Grid.Row>
          <Grid.Column style={{ padding: '4em 0' }} textAlign="center" verticalAlign="middle">
            <LazyImage
              src="/trepl.svg"
              placeholder={({ imageProps, ref }: any) => <img ref={ref} src="/empty.svg" alt={imageProps.alt} />}
              actual={({imageProps}:any) => <img {...imageProps} />}
            />
          </Grid.Column>
        </Grid.Row>
      </Grid>
    </Segment>
    <Footer />
  </ResponsiveContainer>
);

export default HomeView;

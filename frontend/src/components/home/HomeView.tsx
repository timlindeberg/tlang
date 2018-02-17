import * as React from 'react';
import { Button, Container, Grid, Header, List, Segment } from 'semantic-ui-react';
import DesktopContainer from 'components/home/DesktopContainer';
import MobileContainer from 'components/home/MobileContainer';
import CodeBlock from 'components/CodeBlock';

import 'components/home/HomeView.scss';

interface ResponsiveContainerProps {
  children: any;
}

const code =
`package t::lang

val helloWorld = new HelloWorld<Long>(1300L) + 
   new HelloWorld<Long>(37L)

helloWorld.Print()

class HelloWorld<T> =
    
    var times: T
    
    Def new(times: T) = (this.times = times)
    
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
        <Segment style={{ paddingTop: '8em' }} vertical>
        <Grid container stackable verticalAlign="middle"  >
          <Grid.Row>
            <Grid.Column width={8}>
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
            <Grid.Column width={8} verticalAlign="middle">
              <CodeBlock language="tlang">{code}</CodeBlock>
            </Grid.Column>
          </Grid.Row>
          <Grid.Row>
            <Grid.Column style={{ padding: '4em 0' }} textAlign="center" verticalAlign="middle">
              <Button secondary size="huge">Get started</Button>
            </Grid.Column>
          </Grid.Row>
        </Grid>
      </Segment>
      <Segment inverted vertical style={{ padding: '5em 0em' }}>
        <Container>
          <Grid divided inverted stackable>
            <Grid.Row>
              <Grid.Column width={3}>
                <Header inverted as="h4" content="About"/>
                <List link inverted>
                  <List.Item as="a">Sitemap</List.Item>
                  <List.Item as="a">Contact Us</List.Item>
                  <List.Item as="a">Religious Ceremonies</List.Item>
                  <List.Item as="a">Gazebo Plans</List.Item>
                </List>
              </Grid.Column>
              <Grid.Column width={3}>
                <Header inverted as="h4" content="Services"/>
                <List link inverted>
                  <List.Item as="a">Banana Pre-Order</List.Item>
                  <List.Item as="a">DNA FAQ</List.Item>
                  <List.Item as="a">How To Access</List.Item>
                  <List.Item as="a">Favorite X-Men</List.Item>
                </List>
              </Grid.Column>
              <Grid.Column width={7}>
                <Header as="h4" inverted>Footer Header</Header>
                <p>Extra space for a call to action inside the footer that could help re-engage users.</p>
              </Grid.Column>
            </Grid.Row>
          </Grid>
        </Container>
      </Segment>
    </ResponsiveContainer>
);

export default HomeView;

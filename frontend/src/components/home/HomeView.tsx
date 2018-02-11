import * as React from 'react';
import { Button, Container, Divider, Grid, Header, Image, List, Segment } from 'semantic-ui-react';
import DesktopContainer from 'components/home/DesktopContainer';
import MobileContainer from 'components/home/MobileContainer';
import CodeBlock from 'components/CodeBlock';

// import { registerLanguage } from 'react-syntax-highlighter/dist/light';
import 'components/home/HomeView.scss';

interface ResponsiveContainerProps {
  children: any;
}

const code =
`package t::lang

import a::b::c
import d::e::*

class Class<T> : T, B, C =

	var a: Int = 0
	Var b = 1
	val protected c: Long
	Val static d: Float = "hej"

	def implicit new(a: Double, b: String[]) = ;

	/* 
	  An operator 
	*/
	Def +(a: Class<T>, b: Class<T>): Class<T> = ;

	/* A static function */
	def static Func2(): Unit = ;

	// A function  
	Def Func(d: Bool?, e: Char) =
		if(!a < #b && -a <= ~b && a!! > ++b && a++ >= --b)
			println(a + b)
		else
			print(a - b)

		while(a-- == b || a != b)
			error(a * b)

		for(var i = 0; i < 5; i++)
			continue

		for(var x in b)
			break

		Func2(a / b)
		var a = [
		    1 % 1l,
		    1.0 & 1.3e-5f,
		    10 | 5,
		    5.6 ^ 'a',
		    "hej" << true,
		    false >> null,
		]
		
		val x = \`ABC
		DEF
		GHI\`


		A?.Func(a[b])
		this.Func(a[:])
		a.Func(a[  1  :  2  :  3  ])
		super.Func(a[:2])
		a[b] = c as A

		a = a[1:]
		a.b = a[ 1 : 2 : ]
		c = new A[5]
		d = (a is String) ? b : (c ?: 5)
		d = new String(5, 7)
		return d


trait B


extension A
`;

const ResponsiveContainer = ({ children }: ResponsiveContainerProps) => (
    <div>
        <DesktopContainer>{children}</DesktopContainer>
        <MobileContainer>{children}</MobileContainer>
    </div>
);

const HomeView = () => (
    <ResponsiveContainer>
      <Segment style={{ padding: '8em 0em' }} vertical>
        <Grid container stackable verticalAlign="middle">
          <Grid.Row>
            <CodeBlock>{code}</CodeBlock>
          </Grid.Row>
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
                <List.Item>Other cool stuff</List.Item>
              </List>
            </Grid.Column>
          </Grid.Row>
          <Grid.Row>
            <Grid.Column textAlign="center">
              <Button size="huge">Check Them Out</Button>
            </Grid.Column>
          </Grid.Row>
        </Grid>
      </Segment>
      <Segment style={{ padding: '0em' }} vertical>
        <Grid celled="internally" columns="equal" stackable>
          <Grid.Row textAlign="center">
            <Grid.Column style={{ paddingBottom: '5em', paddingTop: '5em' }}>
              <Header as="h3" style={{ fontSize: '2em' }}>"What a Company"</Header>
              <p style={{ fontSize: '1.33em' }}>That is what they all say about us</p>
            </Grid.Column>
            <Grid.Column style={{ paddingBottom: '5em', paddingTop: '5em' }}>
              <Header as="h3" style={{ fontSize: '2em' }}>"I shouldn't have gone with their
                competitor."</Header>
              <p style={{ fontSize: '1.33em' }}>
                <Image avatar src="/assets/images/avatar/large/nan.jpg"/>
                <b>Nan</b> Chief Fun Officer Acme Toys
              </p>
            </Grid.Column>
          </Grid.Row>
        </Grid>
      </Segment>
      <Segment style={{ padding: '8em 0em' }} vertical>
        <Container text>
          <Header as="h3" style={{ fontSize: '2em' }}>Breaking The Grid, Grabs Your Attention</Header>
          <p style={{ fontSize: '1.33em' }}>
            Instead of focusing on content creation and hard work, we have learned how to master the art of
            doing
            nothing by providing massive amounts of whitespace and generic content that can seem massive,
            monolithic
            and worth your attention.
          </p>
          <Button as="a" size="large">Read More</Button>
          <Divider
            as="h4"
            className="header"
            horizontal
            style={{ margin: '3em 0em', textTransform: 'uppercase' }}
          >
            <a href="#">Case Studies</a>
          </Divider>
          <Header as="h3" style={{ fontSize: '2em' }}>Did We Tell You About Our Bananas?</Header>
          <p style={{ fontSize: '1.33em' }}>
            Yes I know you probably disregarded the earlier boasts as non-sequitur filler content, but it's
            really
            true.
            It took years of gene splicing and combinatory DNA research, but our bananas can really dance.
          </p>
          <Button as="a" size="large">I'm Still Quite Interested</Button>
        </Container>
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

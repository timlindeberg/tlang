import 'components/gettingStarted/GettingStartedView.less';
import Footer from 'components/layout/Footer';
import NavbarWithLogo from 'components/layout/NavbarWithLogo';
import CodeBlock from 'components/misc/CodeBlock';
import * as React from 'react';
import Container from 'semantic-ui-react/dist/commonjs/elements/Container/Container';
import Header from 'semantic-ui-react/dist/commonjs/elements/Header/Header';
import List from 'semantic-ui-react/dist/commonjs/elements/List/List';
import Segment from 'semantic-ui-react/dist/commonjs/elements/Segment/Segment';

export default class GettingStartedView extends React.Component<{}, {}> {

  render() {
    return (
      <React.Fragment>
        <NavbarWithLogo />
        <Segment id="GettingStartedView-content">
          <Container text>
            <Header as="h1">Installation</Header>
            <p>The <code>t</code> compiler requires <code>Java 8</code> or later to run.</p>
            <Header as="h3">Mac OSX</Header>
            <p>The compiler and repl is currently hosted in it's own repo since it has too
              few stars to be in the main homebrew repository.</p>
            <List bulleted className="HomeView-larger-text">
              <List.Item>Install homebrew.</List.Item>
              <List.Item>Run <code>brew timlindeberg/tlang/tlang</code>.</List.Item>
            </List>
            <Header as="h3">Linux</Header>
            <List bulleted className="HomeView-larger-text">
              <List.Item>
                Download the latest release at <a href="https://github.com/timlindeberg/tlang/releases">
                https://github.com/timlindeberg/tlang/releases
              </a>.
              </List.Item>
              <List.Item>
                Unpack the zip somewhere.
              </List.Item>
              <List.Item>
                Create a symlink from the bin files in <code>tlang/bin/</code> to <code>/usr/local/bin/</code>.
              </List.Item>
            </List>
            <Header as="h3">Windows</Header>
            <p>
              Windows is currently not offically supported but it might be possible to get it running using <code>
              Cygwin</code>. The compiler can also be started using <code>Java</code> directly.
            </p>
            <Header as="h1">Compiling and running your first program</Header>
            <List bulleted className="HomeView-larger-text">
              <List.Item>
                Create a file called <code>HelloWorld.t</code> and input the following:
                <CodeBlock language="tlang">println("Hello world!")</CodeBlock>
              </List.Item>
              <List.Item>
                Run <code>tcompile HelloWorld.t</code>.
              </List.Item>
              <List.Item>
                This will generate a <code>HelloWorld.class</code> file which can be executed by running <code>java
                HelloWorld</code>.
              </List.Item>
              <List.Item>
                You can also run <code>tcompile --exec HelloWorld.t</code> which will execute the program in addition
                to compiling it.
              </List.Item>
            </List>
          </Container>
        </Segment>
        <Footer bottom={false} />
      </React.Fragment>
    );
  }
}

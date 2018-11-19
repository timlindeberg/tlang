import * as React from 'react';
import Divider from 'semantic-ui-react/dist/commonjs/elements/Divider/Divider';
import Header from 'semantic-ui-react/dist/commonjs/elements/Header/Header';
import Icon from 'semantic-ui-react/dist/commonjs/elements/Icon/Icon';
import Segment from 'semantic-ui-react/dist/commonjs/elements/Segment/Segment';
import TransitionGroup from 'semantic-ui-react/dist/commonjs/modules/Transition/TransitionGroup';
import { scrollTo } from 'utils/misc';
import { PlaygroundEvent } from './events/Events';
import Layout from 'components/layout/Layout';

interface ResultsProps {
  events: PlaygroundEvent[];
}

export default class EventLog extends React.Component<ResultsProps, {}> {

  // The timeout makes sure the element is actually mounted when we try scrolling to it
  scrollTo = (el: any) => {
    if (!el) { return; }

    setTimeout(() => scrollTo(el), 0);
  }

  eventBlock = (event: PlaygroundEvent, key: string, maxToShow: number, shouldScrollTo: boolean): JSX.Element  => {
    const { title, color, icon } = event;
    const body = event.body(maxToShow);
    const scrollFunction = shouldScrollTo ? this.scrollTo : () => { return; };
    return (
      <div key={key} ref={scrollFunction} className="event-block">
        <Segment color={color} className="shadow-hover">
          <Header color={color}>
            <Icon name={icon} />{title}
          </Header>
          {body &&  (<React.Fragment><Divider /> {body}</React.Fragment>)}
        </Segment>
      </div>
    );
  }

  mobileLayout = () => {
    const { events } = this.props;
    return (
      <Segment className="shadow-hover">
        <TransitionGroup duration={300} animation="horizontal flip">
          {events.length > 0 && this.eventBlock(events[events.length - 1], '0', 3, false)}
        </TransitionGroup>
      </Segment>
    );
  }

  desktopLayout = () => {
    const { events } = this.props;
    return (
      <Segment className="PlaygroundView-result-segment shadow-hover">
        <TransitionGroup duration={300} animation="horizontal flip">
          {events.map((e, i) => this.eventBlock(e, i.toString(), 15, i === events.length - 1))}
        </TransitionGroup>
      </Segment>
    );
  }

  render() {
    return <Layout mobile={this.mobileLayout} desktop={this.desktopLayout}/>;
  }

}

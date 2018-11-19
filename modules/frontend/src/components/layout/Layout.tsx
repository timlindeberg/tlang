import * as React from 'react';

interface LayoutProps {
  mobile: () => React.ReactNode;
  desktop: () => React.ReactNode;
}

interface LayoutState {
  screenWidth: number;
}

const MAX_MOBILE = 768;

export default class Layout extends React.Component<LayoutProps, LayoutState> {

  state: LayoutState = { screenWidth: window.innerWidth };

  componentDidMount() {
    window.addEventListener('resize', this.updateWindowDimensions);
  }

  componentWillUnmount() {
    window.removeEventListener('resize', this.updateWindowDimensions);
  }

  updateWindowDimensions = () => this.setState({ screenWidth: window.innerWidth });

  render() {
    const isMobile = this.state.screenWidth <= MAX_MOBILE;
    return isMobile ? this.props.mobile() : this.props.desktop();
  }
}

import * as React from 'react';
import { Accordion, Menu } from 'semantic-ui-react';
import { findFilesWithNames } from 'utils/misc';


interface PlaygroundMenuProps {
  codeExamples: string[];
  updateCode: (s: string) => void;
}

export default class PlaygroundMenu extends React.Component<PlaygroundMenuProps, {}> {

  render() {
    const { updateCode, codeExamples } = this.props;
    return (
      <Accordion
        as={Menu}
        inverted
        borderless
        fluid
        vertical
        size="small"
      >
        <Menu.Item>
          <Accordion.Title active content="Examples"/>
          <Menu.Menu>
            {Object
              .keys(codeExamples)
              .sort((a, b) => a.localeCompare(b, undefined, { numeric: true, sensitivity: 'base' }))
              .map(key => (
                <Menu.Item key={key} onClick={() => updateCode(codeExamples[key])}>{key}</Menu.Item>
              ))}
          </Menu.Menu>
        </Menu.Item>
      </Accordion>
    );
  }


}

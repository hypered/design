import { configure, addDecorator } from "@storybook/react";
import { withStaticMarkup } from "storybook-react-to-static-markup";
import "../static/css/tachyons.css";
import "../static/css/style.css";
import "../static/css/styles.css";

addDecorator(withStaticMarkup);

addDecorator(storyFn => (
  <div className="hy-ibm-plex" style={{ padding: "0" }}>
    {storyFn()}
  </div>
));

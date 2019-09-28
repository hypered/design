import { configure, addDecorator } from "@storybook/react";
import { withInfo } from "@storybook/addon-info";

// automatically import all files ending in *.stories.js
configure(
  [
    require.context("../stories", true, /\.stories\.js$/),
    require.context("../components", true, /\.stories\.js$/),
  ],
  module,
);

addDecorator(
  withInfo({
    inline: true,
  }),
);

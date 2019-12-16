import React from "react";
import { Button } from "../../components";

export default {
  title: "Buttons",
};

export const primary = (props) => <Button variant="primary">Primary Button</Button>;

export const primaryDisabled = (props) => (
  <Button variant="primary" disabled="true">
    Primary Disabled
  </Button>
);

export const secondary = (props) => (
  <Button variant="secondary">Secondary Button</Button>
);

export const secondaryDisabled = (props) => (
  <Button variant="secondary" disabled="true">
    Secondary Disabled Button
  </Button>
);

export const fullWidth = (props) => (
  <Button variant="primary" fullWidth>
    Button Full Width
  </Button>
);

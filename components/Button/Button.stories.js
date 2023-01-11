import React from "react";
import { Button } from "../../components";

export default {
  title: "Buttons",
};

export const primary = props => (
  <Button variant="primary" size="normal">
    Primary Button
  </Button>
);

export const primaryLarge = props => (
  <Button variant="primary" size="large">
    Primary Button
  </Button>
);

export const primaryDisabled = props => (
  <Button variant="primary" size="normal" disabled={true}>
    Primary Disabled
  </Button>
);

export const secondary = props => (
  <Button variant="secondary" size="normal">
    Secondary Button
  </Button>
);

export const secondaryLarge = props => (
  <Button variant="secondary" size="large">
    Secondary Button
  </Button>
);

export const secondaryDisabled = props => (
  <Button variant="secondary" size="normal" disabled={true}>
    Secondary Disabled
  </Button>
);

export const fullWidth = props => (
  <Button variant="primary" size="normal" fullWidth>
    Button Full Width
  </Button>
);

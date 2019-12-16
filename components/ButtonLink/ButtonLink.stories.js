import React from "react";
import { ButtonLink } from "../../components";

export default {
  title: "Button Link",
};

export const primary = () => (
  <ButtonLink variant="primary">Primary Button</ButtonLink>
);

export const primaryDisabled = () => (
  <ButtonLink variant="primary" disabled="true">
    Primary Disabled
  </ButtonLink>
);

export const secondary = () => (
  <ButtonLink variant="secondary">Secondary Button</ButtonLink>
);

export const secondaryDisabled = () => (
  <ButtonLink variant="secondary" disabled="true">
    Secondary Disabled
  </ButtonLink>
);

export const fullWidth = () => (
  <ButtonLink variant="primary" fullWidth>
    Button Full Width
  </ButtonLink>
);

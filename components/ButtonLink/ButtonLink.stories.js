import React from "react";
import { ButtonLink } from "../../components";

export default {
  title: "Button Link",
};

export const primary = () => (
  <ButtonLink variant="primary" size="normal">
    Primary Button
  </ButtonLink>
);

export const primaryLarge = () => (
  <ButtonLink variant="primary" size="large">
    Primary Button
  </ButtonLink>
);

export const primaryDisabled = () => (
  <ButtonLink variant="primary" size="normal" disabled="true">
    Primary Disabled
  </ButtonLink>
);

export const secondary = () => (
  <ButtonLink variant="secondary" size="normal">
    Secondary Button
  </ButtonLink>
);

export const secondaryLarge = () => (
  <ButtonLink variant="secondary" size="large">
    Secondary Button
  </ButtonLink>
);

export const secondaryDisabled = () => (
  <ButtonLink variant="secondary" size="normal" disabled="true">
    Secondary Disabled
  </ButtonLink>
);

export const fullWidth = () => (
  <ButtonLink variant="primary" size="normal" fullWidth>
    Button Full Width
  </ButtonLink>
);

import React from "react";
import {
  ButtonLinkPrimary,
  ButtonLinkPrimaryDisabled,
  ButtonLinkSecondary,
  ButtonLinkSecondaryDisabled,
  ButtonLinkFullWidth,
} from "../../components";

export default {
  title: "Button Link",
};

export const primary = () => (
  <ButtonLinkPrimary>Primary Button</ButtonLinkPrimary>
);

export const primaryDisabled = () => (
  <ButtonLinkPrimaryDisabled>Primary Disabled</ButtonLinkPrimaryDisabled>
);

export const secondary = () => (
  <ButtonLinkSecondary>Secondary Button</ButtonLinkSecondary>
);

export const secondaryDisabled = () => (
  <ButtonLinkSecondaryDisabled>
    Secondary Disabled Button
  </ButtonLinkSecondaryDisabled>
);

export const fullWidth = () => (
  <ButtonLinkFullWidth>Button Full Width</ButtonLinkFullWidth>
);

import React from "react";
import {
  ButtonPrimary,
  ButtonPrimaryDisabled,
  ButtonSecondary,
  ButtonSecondaryDisabled,
  ButtonFullWidth,
} from "../../components";

export default {
  title: "Button",
};

export const primary = () => <ButtonPrimary>Primary Button</ButtonPrimary>;

export const primaryDisabled = () => (
  <ButtonPrimaryDisabled>Primary Disabled</ButtonPrimaryDisabled>
);

export const secondary = () => (
  <ButtonSecondary>Secondary Button</ButtonSecondary>
);

export const secondaryDisabled = () => (
  <ButtonSecondaryDisabled>Secondary Disabled Button</ButtonSecondaryDisabled>
);

export const fullWidth = () => (
  <ButtonFullWidth>Button Full Width</ButtonFullWidth>
);

import React from "react";
import {
  ButtonPrimary,
  ButtonPrimaryDisabled,
  ButtonSecondary,
  ButtonSecondaryDisabled,
} from "../../components";

export default {
  title: "Button",
};

export const primary = () => <ButtonPrimary>Primary Button</ButtonPrimary>;

export const primaryDisabled = () => (
  <ButtonPrimaryDisabled>Disabled</ButtonPrimaryDisabled>
);

export const secondary = () => (
  <ButtonSecondary>Primary Button</ButtonSecondary>
);

export const secondaryDisabled = () => (
  <ButtonSecondaryDisabled>Primary Button</ButtonSecondaryDisabled>
);

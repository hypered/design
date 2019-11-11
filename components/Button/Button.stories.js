import React from "react";
import {
  ButtonPrimary,
  ButtonPrimaryDisabled,
  ButtonPrimaryPill,
  ButtonPrimaryPillDisabled,
  ButtonSecondary,
  ButtonSecondaryDisabled,
  ButtonSecondaryPill,
  ButtonSecondaryPillDisabled,
  ButtonFullWidth,
} from "../../components";

export default {
  title: "Button",
};

export const primary = () => <ButtonPrimary>Primary Button</ButtonPrimary>;

export const primaryDisabled = () => (
  <ButtonPrimaryDisabled>Primary Disabled</ButtonPrimaryDisabled>
);

export const primaryPill = () => (
  <ButtonPrimaryPill>Primary Pill Button</ButtonPrimaryPill>
);

export const primaryPillDisabled = () => (
  <ButtonPrimaryPillDisabled>
    Primary Pill Disabled Button
  </ButtonPrimaryPillDisabled>
);

export const secondary = () => (
  <ButtonSecondary>Secondary Button</ButtonSecondary>
);

export const secondaryDisabled = () => (
  <ButtonSecondaryDisabled>Secondary Disabled Button</ButtonSecondaryDisabled>
);

export const secondaryPill = () => (
  <ButtonSecondaryPill>Secondary Pill Button</ButtonSecondaryPill>
);

export const secondaryPillDisabled = () => (
  <ButtonSecondaryPillDisabled>
    Secondary Pill Disabled Button
  </ButtonSecondaryPillDisabled>
);

export const fullWidth = () => (
  <ButtonFullWidth>Button Full Width</ButtonFullWidth>
);

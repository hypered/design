import React from "react";
import {
  ButtonLinkPrimary,
  ButtonLinkPrimaryDisabled,
  ButtonLinkPrimaryPill,
  ButtonLinkPrimaryPillDisabled,
  ButtonLinkSecondary,
  ButtonLinkSecondaryDisabled,
  ButtonLinkSecondaryPill,
  ButtonLinkSecondaryPillDisabled,
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

export const primaryPill = () => (
  <ButtonLinkPrimaryPill>Primary Pill Button</ButtonLinkPrimaryPill>
);

export const primaryPillDisabled = () => (
  <ButtonLinkPrimaryPillDisabled>
    Primary Pill Disabled Button
  </ButtonLinkPrimaryPillDisabled>
);

export const secondary = () => (
  <ButtonLinkSecondary>Secondary Button</ButtonLinkSecondary>
);

export const secondaryDisabled = () => (
  <ButtonLinkSecondaryDisabled>
    Secondary Disabled Button
  </ButtonLinkSecondaryDisabled>
);

export const secondaryPill = () => (
  <ButtonLinkSecondaryPill>Secondary Pill Button</ButtonLinkSecondaryPill>
);

export const secondaryPillDisabled = () => (
  <ButtonLinkSecondaryPillDisabled>
    Secondary Pill Disabled Button
  </ButtonLinkSecondaryPillDisabled>
);

export const fullWidth = () => (
  <ButtonLinkFullWidth>Button Full Width</ButtonLinkFullWidth>
);

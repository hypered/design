import React, { useState } from "react";
import { Checkbox } from "../../components";

export default {
  title: "Checkboxes",
};

export const Default = () => {
  const checked = true;

  return (
    <Checkbox
      type="checkbox"
      defaultChecked={checked}
      label="This is checked"
    />
  );
};

export const Pill = () => {
  const checked = true;

  return (
    <Checkbox
      type="checkbox"
      defaultChecked={checked}
      label="This is checked"
      pill={true}
    />
  );
};

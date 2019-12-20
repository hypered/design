import React, { useState } from "react";
import { Checkbox } from "../../components";

export default {
  title: "Checkboxes",
};

export const Default = () => {
  const [checked, setChecked] = useState(true);

  function handleChecked(e) {
    setChecked(e.target.checked);
  }

  return (
    <Checkbox
      type="checkbox"
      checked={checked}
      label={`This is ${checked ? "checked" : "unchecked"}`}
      onChange={e => handleChecked(e)}
    />
  );
};

export const CheckboxPill = () => {
  const [checked, setChecked] = useState(true);

  function handleChecked(e) {
    setChecked(e.target.checked);
  }

  return (
    <Checkbox
      type="checkbox"
      checked={checked}
      label={`This is ${checked ? "checked" : "false"}`}
      onChange={e => handleChecked(e)}
      pill={true}
    />
  );
};

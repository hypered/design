import React, { useState } from "react";
import { Checkbox } from "../../components";

export default {
  title: "Checkbox",
};

export const CheckboxUnchecked = () => {
  const [checked, setChecked] = useState(true);

  function handleChecked(e) {
    setChecked(e.target.checked);
  }

  return (
    <Checkbox
      checked={checked}
      label={`This is ${checked ? "checked" : "unchecked"}`}
      onChange={e => handleChecked(e)}
    />
  );
};

export const CheckboxSelectedPill = () => {
  const [checked, setChecked] = useState(true);

  function handleChecked(e) {
    setChecked(e.target.checked);
  }

  return (
    <Checkbox
      checked={checked}
      label={`This is ${checked ? "checked" : "false"}`}
      onChange={e => handleChecked(e)}
      pill={true}
    />
  );
};

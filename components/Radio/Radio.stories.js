import React, { useState } from "react";
import { Radio, Checkbox } from "../../components";

export default {
  title: "Radio",
};

const options = [
  { id: "apple", name: "Apple" },
  { id: "banana", name: "Banana" },
  { id: "cherry", name: "Cherry" },
];

export const Default = ({ props }) => {
  const [checked, setChecked] = useState("apple");

  function handleChecked(e) {
    setChecked(e.target.value);
  }

  return (
    <Radio>
      {options.map((option, i) => (
        <Checkbox
          type="radio"
          pill
          key={option.id}
          checked={checked === option.id}
          label={option.name}
          value={option.id}
          onChange={e => handleChecked(e)}
          name="fruits"
        />
      ))}
    </Radio>
  );
};

export const RadioPillInline = () => {
  const [checked, setChecked] = useState("apple");

  function handleChecked(e) {
    setChecked(e.target.value);
  }

  return (
    <Radio inline>
      {options.map((option, i) => (
        <Checkbox
          type="radio"
          pill
          inline
          key={option.id}
          checked={checked === option.id}
          label={option.name}
          value={option.id}
          onChange={e => handleChecked(e)}
          name="fruits"
        />
      ))}
    </Radio>
  );
};

export const RadioCheckbox = () => {
  const [checked, setChecked] = useState("apple");

  function handleChecked(e) {
    setChecked(e.target.value);
  }

  return (
    <Radio>
      {options.map((option, i) => (
        <Checkbox
          type="radio"
          key={option.id}
          checked={checked === option.id}
          label={option.name}
          value={option.id}
          onChange={e => handleChecked(e)}
          name="fruits"
        />
      ))}
    </Radio>
  );
};

export const RadioCheckboxInline = () => {
  const [checked, setChecked] = useState("apple");

  function handleChecked(e) {
    setChecked(e.target.value);
  }

  return (
    <Radio inline>
      {options.map((option, i) => (
        <Checkbox
          type="radio"
          inline
          key={option.id}
          checked={checked === option.id}
          label={option.name}
          value={option.id}
          onChange={e => handleChecked(e)}
          name="fruits"
        />
      ))}
    </Radio>
  );
};

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

export const Default = () => {
  const checked = "apple";

  return (
    <Radio>
      {options.map((option, i) => (
        <Checkbox
          type="radio"
          pill
          key={option.id}
          defaultChecked={checked === option.id}
          label={option.name}
          value={option.id}
          name="fruits"
        />
      ))}
    </Radio>
  );
};

export const RadioPillInline = () => {
  const checked = "apple";

  return (
    <Radio inline>
      {options.map((option, i) => (
        <Checkbox
          type="radio"
          pill
          inline
          key={option.id}
          defaultChecked={checked === option.id}
          label={option.name}
          value={option.id}
          name="fruits"
        />
      ))}
    </Radio>
  );
};

export const RadioCheckbox = () => {
  const checked = "apple";

  return (
    <Radio>
      {options.map((option, i) => (
        <Checkbox
          type="radio"
          key={option.id}
          defaultChecked={checked === option.id}
          label={option.name}
          value={option.id}
          name="fruits"
        />
      ))}
    </Radio>
  );
};

export const RadioCheckboxInline = () => {
  const checked = "apple";

  return (
    <Radio inline>
      {options.map((option, i) => (
        <Checkbox
          type="radio"
          inline
          key={option.id}
          defaultChecked={checked === option.id}
          label={option.name}
          value={option.id}
          name="fruits"
        />
      ))}
    </Radio>
  );
};

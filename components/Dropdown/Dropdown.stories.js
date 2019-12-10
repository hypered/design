import React, { useState } from "react";
import { Dropdown, Option } from "../../components";

export default {
  title: "Dropdowns",
};

export const Default = () => {
  const [value, setValue] = useState();

  function handleChange(e) {
    setValue(e.target.value);
  }

  return (
    <Dropdown value={value} onChange={handleChange}>
      <Option value="" defaultValue>
        Select from dropdown
      </Option>
      <Option value="apple">Apple</Option>
      <Option value="banana">Banana</Option>
      <Option value="cherry">Cherry</Option>
    </Dropdown>
  );
};

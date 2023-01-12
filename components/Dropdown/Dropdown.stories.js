import React, { useState } from "react";
import { Dropdown, Option } from "../../components";

export default {
  title: "Dropdowns",
};

export const Default = () => {
  return (
    <Dropdown>
      <Option value="" defaultValue>
        Select from dropdown
      </Option>
      <Option value="apple">Apple</Option>
      <Option value="banana">Banana</Option>
      <Option value="cherry">Cherry</Option>
    </Dropdown>
  );
};

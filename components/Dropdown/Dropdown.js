import React from "react";
import cx from "classnames";

export const Option = props => (
  <option
    name={props.name}
    value={props.value}
    defaultValue={props.defaultValue}
  >
    {props.children}
  </option>
);

export const Dropdown = props => {
  return (
    <select
      className="hy-dropdown br1 black ba b--black bw1 br0 pv2 f6"
      {...props}
    >
      {props.children}
    </select>
  );
};

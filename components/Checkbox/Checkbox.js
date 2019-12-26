import React from "react";
import cx from "classnames";

export const Checkbox = props => {
  let labelClassNames = cx("flex", "items-center", {
    mr3: props.inline,
    mb2: !props.inline,
    flex: !props.inline,
    "inline-flex": props.inline,
  });

  let checkboxClassNames = cx("hy-checkbox", "w1", "h1", {
    "br-pill": props.pill,
  });

  return (
    <label className={labelClassNames}>
      <input
        className={checkboxClassNames}
        name={props.name}
        type={props.type}
        role={props.role}
        checked={props.checked}
        onChange={props.onChange}
        defaultChecked={props.defaultChecked}
        pill={props.pill}
        value={props.value}
      />
      <div className="ml1">{props.label}</div>
    </label>
  );
};

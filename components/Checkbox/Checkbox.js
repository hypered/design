import React from "react";
import cx from "classnames";

export const Checkbox = props => {
  let checkboxClassNames = cx("checkbox", "w1", "h1", {
    "br-pill": props.pill,
  });

  return (
    <div>
      <label className="inline-flex items-center">
        <input
          className={checkboxClassNames}
          type="checkbox"
          role="checkbox"
          checked={props.checked}
          onChange={props.onChange}
          defaultChecked={props.defaultChecked}
          pill={props.pill}
        />
        <div className="ml2">{props.label}</div>
      </label>
    </div>
  );
};

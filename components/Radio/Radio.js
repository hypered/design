import React from "react";
import cx from "classnames";
import { Checkbox } from "../";

export const Radio = props => {
  let radioClassNames = cx({
    flex: props.inline,
    "items-center": props.inline,
    db: !props.inline,
  });

  return <div className={radioClassNames}>{props.children}</div>;
};

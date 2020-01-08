import React from "react";
import cx from "classnames";

export const A = ({ color, children, ...props }) => {
  let aClassNames = cx(
    {
      "hy-blue": color === "blue",
      black: color === "black",
    },
    "no-underline",
    "hy-hover-blue",
  );
  return (
    <a className={aClassNames} {...props}>
      {children}
    </a>
  );
};

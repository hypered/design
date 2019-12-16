import React from "react";
import cx from "classnames";

export const Button = props => {
  let buttonClassNames = cx(
    {
      "bg-black": props.variant === "primary",
      "b--black": props.variant === "primary",
      white: props.variant === "primary",
    },
    {
      "bg-white": props.variant === "secondary",
      "b--black": props.variant === "secondary",
      black: props.variant === "secondary",
    },
    {
      "o-50": props.disabled,
    },
    {
      "w-100": props.fullWidth,
    },
    "button-reset",
    "ph4",
    "pv3",
    "ba",
    "bw1",
  );

  return <button className={buttonClassNames}>{props.children}</button>;
};

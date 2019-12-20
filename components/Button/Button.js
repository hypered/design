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
    { ph4: props.size === "normal", pv3: props.size === "normal" },
    {
      ph3: props.size === "large",
      pb4: props.size === "large",
      pt3: props.size === "large",
      tl: props.size === "large",
    },
    {
      "o-50": props.disabled,
    },
    {
      "w-100": props.fullWidth || props.size === "large",
    },
    "button-reset",
    "ba",
    "bw1",
  );

  return <button className={buttonClassNames}>{props.children}</button>;
};

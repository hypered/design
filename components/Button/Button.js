import React from "react";
import cx from "classnames";

export const Button = props => {
  const ButtonType = props.as ? `${props.as}` : "button";

  let buttonClassNames = cx(
    {
      "bg-black": props.variant === "primary",
      "b--black": props.variant === "primary",
      white: props.variant === "primary",
      "hover-light-green": props.variant === "primary"
    },
    {
      "bg-white": props.variant === "secondary",
      "hover-bg-light-gray": props.variant === "secondary",
      "b--black": props.variant === "secondary",
      black: props.variant === "secondary"
    },
    { ph4: props.size === "normal", pv3: props.size === "normal" },
    {
      ph3: props.size === "large",
      pb4: props.size === "large",
      pt3: props.size === "large",
      tl: props.size === "large"
    },
    {
      "o-50": props.disabled
    },
    {
      "w-100": props.fullWidth || props.size === "large"
    },
    "pointer",
    "inline-flex",
    "button-reset",
    "ba",
    "bw1",
    "relative"
  );

  return (
    <ButtonType className={buttonClassNames} {...props}>
      {props.children}
    </ButtonType>
  );
};

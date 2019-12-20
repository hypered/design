import React from "react";
import cx from "classnames";

export const Banner = props => {
  const bannerClassNames = cx(
    "bg-black",
    "pa3",
    "white",
    "tc",
    "fw6",
    "mv3",
    "bl",
    "bw3",
    {
      "b--green": props.color === "green",
    },
    {
      "b--yellow": props.color === "yellow",
    },
    {
      "b--red": props.color === "red",
    },
  );
  return (
    <div className={bannerClassNames} color={props.color}>
      {props.children}
    </div>
  );
};

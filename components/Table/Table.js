import React from "react";
import cx from "classnames";

export const Table = props => (
  <div className="overflow-x-scroll">
    <table className="bg-white collapse w-100" style={{ minWidth: "40rem" }}>
      {props.children}
    </table>
  </div>
);

export const TH = ({ size, align, children }) => {
  let thClassNames = cx(
    "tl",
    "bb",
    "bw1",
    "fw6",
    "nowrap",
    {
      f5: size === "normal" || !size,
      pa2: size === "normal" || !size,
      "b--black": size === "normal" || !size
    },
    {
      f6: size === "compact",
      pa1: size === "compact",
      "b--silver": size === "compact"
    },
    {
      tl: align === "left" || !align,
      tr: align === "right",
      tc: align === "center"
    }
  );

  return (
    <th className={thClassNames} size={size} align={align}>
      {children}
    </th>
  );
};

export const TD = ({ size, align, children }) => {
  let tdClassNames = cx(
    "bb",
    "nowrap",
    {
      f5: size === "normal" || !size,
      pa2: size === "normal" || !size,
      "b--black": size === "normal" || size
    },
    {
      f6: size === "compact",
      pa1: size === "compact",
      "b--silver": size === "compact"
    },
    {
      tl: align === "left" || !align,
      tr: align === "right",
      tc: align === "center"
    }
  );

  return (
    <td className={tdClassNames} size={size} align={align}>
      {children}
    </td>
  );
};

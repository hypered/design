import React from "react";
import cx from "classnames";

export const Table = ({ children }) => (
  <div className="overflow-x-scroll">
    <table className="bg-white collapse w-100">{children}</table>
  </div>
);

export const TR = ({ isHeader, size, hideBottomBorder, children }) => {
  let trClassNames = cx("b--black", {
    bw1: isHeader,
    bb: !hideBottomBorder,
    "b--black": size === "normal" || !size,
    "b--silver": size === "compact"
  });
  return (
    <tr
      className={trClassNames}
      size={size}
    >
      {children}
    </tr>
  );
};

export const TH = ({ size, align, colDivider, children }) => {
  let thClassNames = cx(
    "tl",
    "fw6",
    "nowrap",
    {
      f5: size === "normal" || !size,
      pa2: size === "normal" || !size
    },
    {
      f6: size === "compact",
      pa1: size === "compact"
    },
    {
      tl: align === "left" || !align,
      tr: align === "right",
      tc: align === "center"
    },
    {
      br: colDivider
    }
  );

  return (
    <th
      className={thClassNames}
      size={size}
      align={align}
    >
      {children}
    </th>
  );
};

export const TD = ({ size, align, colDivider, children }) => {
  let tdClassNames = cx(
    "nowrap",
    {
      f5: size === "normal" || !size,
      pa2: size === "normal" || !size
    },
    {
      f6: size === "compact",
      pa1: size === "compact"
    },
    {
      tl: align === "left" || !align,
      tr: align === "right",
      tc: align === "center"
    },
    {
      br: colDivider
    }
  );

  return (
    <td
      className={tdClassNames}
      size={size}
      align={align}
    >
      {children}
    </td>
  );
};

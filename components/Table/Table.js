import React from "react";

export const Table = props => (
  <div className="overflow-x-scroll">
    <table className="bg-white collapse w-100" style={{ minWidth: "40rem" }}>
      {props.children}
    </table>
  </div>
);

export const TH1 = props => (
  <th className="tl bb pa2 fw6 nowrap">{props.children}</th>
);

export const TH2 = props => (
  <th className="tl bb pa1 fw6 f6 nowrap">{props.children}</th>
);

export const TD1 = props => (
  <td className="bb b--black pa2 nowrap">{props.children}</td>
);

export const TD2 = props => (
  <td className="bb b--silver pa1 f6 nowrap">{props.children}</td>
);

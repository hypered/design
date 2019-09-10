import React from "react";

function TD1(props) {
  return <td className="bb b--black pa2 nowrap">{props.children}</td>;
}

function TD2(props) {
  return <td className="bb b--silver pa1 f6 nowrap">{props.children}</td>;
}

export { TD1, TD2 };

import React from "react";

import { Nav } from "../components";

function Layout(props) {
  return (
    <div className="pa4">
      <Nav></Nav>
      {props.children}
    </div>
  );
}

export { Layout };

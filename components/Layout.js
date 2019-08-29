import React from "react";

import { Nav } from "../components";

function Layout(props) {
  return (
    <div className="pa4">
      <Nav></Nav>
      <main>{props.children}</main>
    </div>
  );
}

export { Layout };

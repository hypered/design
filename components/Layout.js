import React from "react";

import { Nav, Footer } from "../components";

function Layout(props) {
  return (
    <div className="mw8 center pa4">
      <Nav></Nav>
      <main>{props.children}</main>
      <Footer></Footer>
    </div>
  );
}

export { Layout };

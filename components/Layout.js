import React from "react";

import { Nav, Footer } from "../components";

function Layout(props) {
  return (
    <div className="mw8 center pa4 lh-copy">
      <Nav></Nav>
      <main>{props.children}</main>
      <Footer></Footer>
    </div>
  );
}

export { Layout };

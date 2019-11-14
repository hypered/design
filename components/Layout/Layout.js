import React from "react";

import { Nav, NavLink, Footer } from "../../components";

function Layout(props) {
  return (
    <div className="mw8 center pa4 lh-copy">
      <Nav>
        <NavLink active={true}>Item 1</NavLink>
        <NavLink>Item 2</NavLink>
        <NavLink>Item 3</NavLink>
      </Nav>
      {props.children}
      <Footer></Footer>
    </div>
  );
}

export { Layout };

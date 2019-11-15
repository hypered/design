import React from "react";

import { Nav, NavLink, Footer } from "../../components";

function Layout(props) {
  return (
    <div className="mw8 center pa4 lh-copy">
      <Nav>
        <NavLink href="#" active={true}>
          Item 1
        </NavLink>
        <NavLink href="#">Item 2</NavLink>
        <NavLink href="#">Item 3</NavLink>
      </Nav>
      {props.children}
      <Footer></Footer>
    </div>
  );
}

export { Layout };

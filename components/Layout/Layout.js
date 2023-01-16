import React from "react";

import { Nav, NavLink, Footer } from "../../components";

function BareLayout(props) {
  return (
    <div className="flex flex-column justify-between min-vh-100 mw8 center pa3 pa4-ns lh-copy">
      <div>
        {props.children}
      </div>
    </div>
  );
}

function Layout(props) {
  return (
    <div className="flex flex-column justify-between min-vh-100 mw8 center pa3 pa4-ns lh-copy">
      <div>
        <header>
          <Nav>
            <div>
              <NavLink href="#" active={true}>
                noteed.com
              </NavLink>
              <NavLink href="#">blog</NavLink>
              <NavLink href="#">not-os</NavLink>
            </div>
          </Nav>
        </header>
        {props.children}
      </div>
      <Footer copyright={props.copyright}></Footer>
    </div>
  );
}

export { BareLayout, Layout };

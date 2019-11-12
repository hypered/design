import React from "react";
import { withRouter } from "next/router";
import Link from "next/link";

import { Nav, NavLink } from "../../components";

const NavItem = withRouter(({ children, href, router }) => {
  const { pathname, query } = href;

  let active = router.pathname === (pathname || href);

  return (
    <Link href={href} passHref>
      <NavLink active={active}>{children}</NavLink>
    </Link>
  );
});

const NavSection = props => (
  <Nav>
    <NavItem href="/">Home</NavItem>
    <NavItem href="/components">Components</NavItem>
  </Nav>
);

export { NavSection };

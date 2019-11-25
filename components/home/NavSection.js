import React from "react";
import { withRouter } from "next/router";
import Link from "next/link";

import { Nav, NavLink } from "../../components";

const NavItem = withRouter(({ children, href, router }) => {
  const { pathname, query } = href;

  let active = router.pathname === (pathname || href);

  return (
    <Link href={href} passHref as={process.env.BACKEND_URL + href}>
      <NavLink active={active ? 1 : null}>{children}</NavLink>
    </Link>
  );
});

const NavSection = props => (
  <Nav>
    <div>
      <NavItem href="/">Home</NavItem>
      <NavItem href="/components">Components</NavItem>
    </div>
  </Nav>
);

export { NavSection };

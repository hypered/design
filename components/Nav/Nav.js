import React from "react";
import Link from "next/link";

const NavLink = () => (
  <Link href={props.href}>
    <a className="link mr3 black hover-blue">{props.children}</a>
  </Link>
);

export const Nav = () => (
  <header className="flex mb4">
    <nav className="flex align-items-center lh-copy">
      <NavLink href="#">Item 1</NavLink>
      <NavLink href="#">Item 2</NavLink>
      <NavLink href="#">Item 3</NavLink>
    </nav>
  </header>
);

import React from "react";
import Link from "next/link";

function NavLink(props) {
  return (
    <Link href={props.href}>
      <a className="link mr3 black hover-blue">{props.children}</a>
    </Link>
  );
}

function Nav() {
  return (
    <header className="flex mb4">
      <nav className="flex align-items-center lh-copy">
        <NavLink href="/">noteed.com</NavLink>
        <NavLink href="/blog">blog</NavLink>
        <NavLink href="/projects/not-os">not-os</NavLink>
      </nav>

      <nav className="flex align-items-center lh-copy ml4">
        <NavLink href="/sprint-2">sprint-2</NavLink>
      </nav>
    </header>
  );
}

export { Nav };

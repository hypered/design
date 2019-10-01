import Link from "next/link";

export const SidebarTitle = props => (
  <h3 className="f5 ttu mv1">{props.children}</h3>
);

export const SidebarUL = props => (
  <ul className="list pl0 mb3 mt0">{props.children}</ul>
);

export const SidebarLI = props => <li>{props.children}</li>;

export const SidebarLink = props => (
  <Link href={props.href}>
    <a className="link black hover-blue">{props.children}</a>
  </Link>
);

export const Sidebar = () => (
  <nav className="order-1 order-0-m order-0-l w-100 w-25-m w-25-l pv3 ph3">
    <SidebarTitle>Section 1</SidebarTitle>
    <SidebarUL>
      <SidebarLI>
        <SidebarLink href="#">Item One</SidebarLink>
      </SidebarLI>
      <SidebarLI>
        <SidebarLink href="#">Item Two</SidebarLink>
      </SidebarLI>
      <SidebarLI>
        <SidebarLink href="#">Item Three</SidebarLink>
      </SidebarLI>
    </SidebarUL>

    <SidebarTitle>Section 2</SidebarTitle>
    <SidebarUL>
      <SidebarLI>
        <SidebarLink href="#">Item One</SidebarLink>
      </SidebarLI>
      <SidebarLI>
        <SidebarLink href="#">Item Two</SidebarLink>
      </SidebarLI>
      <SidebarLI>
        <SidebarLink href="#">Item Three</SidebarLink>
      </SidebarLI>
    </SidebarUL>

    <SidebarTitle>Section 3</SidebarTitle>
    <SidebarUL>
      <SidebarLI>
        <SidebarLink href="#">Item One</SidebarLink>
      </SidebarLI>
      <SidebarLI>
        <SidebarLink href="#">Item Two</SidebarLink>
      </SidebarLI>
      <SidebarLI>
        <SidebarLink href="#">Item Three</SidebarLink>
      </SidebarLI>
    </SidebarUL>
  </nav>
);

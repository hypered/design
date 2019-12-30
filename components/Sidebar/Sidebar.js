import Link from "next/link";
import { A } from "../../components";

export const SidebarTitle = props => (
  <h3 className="f5 ttu mv1">{props.children}</h3>
);

export const SidebarUL = props => (
  <ul className="list pl0 mb3 mt0">{props.children}</ul>
);

export const SidebarLI = props => <li>{props.children}</li>;

export const SidebarLink = props => (
  <Link href={props.href}>
    <A color="black">{props.children}</A>
  </Link>
);

export const Sidebar = () => (
  <aside className="order-2 order-0-m order-0-l w-100 w-20-m w-20-l ph3 mt2">
    <nav>
      <SidebarTitle>Intro</SidebarTitle>
      <SidebarUL>
        <SidebarLI>
          <SidebarLink href="#">not-os</SidebarLink>
        </SidebarLI>
      </SidebarUL>

      <SidebarTitle>Notes</SidebarTitle>
      <SidebarUL>
        <SidebarLI>
          <SidebarLink href="#">Digital Ocean</SidebarLink>
        </SidebarLI>
        <SidebarLI>
          <SidebarLink href="#">TODO</SidebarLink>
        </SidebarLI>
      </SidebarUL>

      <SidebarTitle>Values</SidebarTitle>
      <SidebarUL>
        <SidebarLI>
          <SidebarLink href="#">command-line</SidebarLink>
        </SidebarLI>
        <SidebarLI>
          <SidebarLink href="#">root-modules</SidebarLink>
        </SidebarLI>
      </SidebarUL>
    </nav>
  </aside>
);

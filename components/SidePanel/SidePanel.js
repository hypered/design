import Link from "next/link";

export const SidePanelTitle = props => (
  <h3 className="f5 lh-title mv2">{props.children}</h3>
);

export const SidePanelUL = props => (
  <ul className="bg-near-white list pa3">{props.children}</ul>
);

export const SidePanelLI = props => (
  <li className="pv1 bb b--black-10">{props.children}</li>
);

export const SidePanelLink = props => (
  <Link href={props.href}>
    <a className="link no-underline black blue-hover">
      &rarr; {props.children}
    </a>
  </Link>
);

export const SidePanel = props => (
  <aside className="order-1 order-2-m order-2-l w-100 w-20-m w-20-l ph3 mt2">
    <div className="">{props.children}</div>
  </aside>
);

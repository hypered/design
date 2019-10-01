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
  <aside className="w-100 w-20-m w-20-l ph3 mt0 mt5-m mt5-l">
    <div className="nl3 nr3">{props.children}</div>
  </aside>
);

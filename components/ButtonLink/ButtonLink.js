export const ButtonLinkPrimary = props => (
  <a
    className="no-underline dib tc ph4 pv3 bg-black white ba bw1 b--black"
    href={props.href}
  >
    {props.children}
  </a>
);

export const ButtonLinkPrimaryDisabled = props => (
  <a
    className="no-underline dib tc ph4 pv3 bg-black white ba bw1 b--black o-50"
    href={props.href}
  >
    {props.children}
  </a>
);

export const ButtonLinkPrimaryPill = props => (
  <a
    className="no-underline dib tc ph4 pv3 bg-black white ba bw1 b--black br-pill"
    href={props.href}
  >
    {props.children}
  </a>
);

export const ButtonLinkPrimaryPillDisabled = props => (
  <a
    className="no-underline dib tc ph4 pv3 bg-black white ba bw1 b--black o-50 br-pill"
    href={props.href}
  >
    {props.children}
  </a>
);

export const ButtonLinkSecondary = props => (
  <a
    className="no-underline dib tc ph4 pv3 bg-white black ba b--black bw1"
    href={props.href}
  >
    {props.children}
  </a>
);

export const ButtonLinkSecondaryDisabled = props => (
  <a
    className="no-underline dib tc ph4 pv3 bg-white black ba b--black bw1 o-50"
    href={props.href}
  >
    {props.children}
  </a>
);

export const ButtonLinkSecondaryPill = props => (
  <a
    className="no-underline dib tc ph4 pv3 bg-white black ba b--black bw1 br-pill"
    href={props.href}
  >
    {props.children}
  </a>
);

export const ButtonLinkSecondaryPillDisabled = props => (
  <a
    className="no-underline dib tc ph4 pv3 bg-white black ba b--black bw1 o-50 br-pill"
    href={props.href}
  >
    {props.children}
  </a>
);

export const ButtonLinkFullWidth = props => (
  <a
    className="no-underline dib tc ph4 pv3 bg-black white ba bw1 b--black w-100"
    href={props.href}
  >
    {props.children}
  </a>
);

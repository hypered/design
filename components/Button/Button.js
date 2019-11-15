export const ButtonPrimary = props => (
  <button className="button-reset ph4 pv3 bg-black white ba bw1 b--black">
    {props.children}
  </button>
);

export const ButtonPrimaryDisabled = props => (
  <button
    className="button-reset ph4 pv3 bg-black white ba bw1 b--black o-50"
    disabled
  >
    {props.children}
  </button>
);

export const ButtonPrimaryPill = props => (
  <button className="button-reset ph4 pv3 bg-black white ba bw1 b--black br-pill">
    {props.children}
  </button>
);

export const ButtonPrimaryPillDisabled = props => (
  <button
    className="button-reset ph4 pv3 bg-black white ba bw1 b--black o-50 br-pill"
    disabled
  >
    {props.children}
  </button>
);

export const ButtonSecondary = props => (
  <button className="button-reset ph4 pv3 bg-white black ba b--black bw1">
    {props.children}
  </button>
);

export const ButtonSecondaryDisabled = props => (
  <button
    className="button-reset ph4 pv3 bg-white black ba b--black bw1 o-50"
    disabled
  >
    {props.children}
  </button>
);

export const ButtonSecondaryPill = props => (
  <button className="button-reset ph4 pv3 bg-white black ba b--black bw1 br-pill">
    {props.children}
  </button>
);

export const ButtonSecondaryPillDisabled = props => (
  <button
    className="button-reset ph4 pv3 bg-white black ba b--black bw1 o-50 br-pill"
    disabled
  >
    {props.children}
  </button>
);

export const ButtonFullWidth = props => (
  <button className="button-reset ph4 pv3 bg-black white ba bw1 b--black w-100">
    {props.children}
  </button>
);

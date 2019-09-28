import React from "react";
import "../../static/css/styles.css";
import "../../static/css/tachyons.css";

function ButtonPrimary(props) {
  return (
    <button className="inter f5 button-reset ph4 pv3 bg-black white ba bw1 b--black">
      {props.children}
    </button>
  );
}

function ButtonPrimaryDisabled(props) {
  return (
    <button
      className="inter button-reset ph4 pv3 bg-black white ba bw1 b--black o-50"
      disabled
    >
      {props.children}
    </button>
  );
}

function ButtonSecondary(props) {
  return (
    <button className="inter button-reset ph4 pv3 bg-white black ba b--black bw1">
      {props.children}
    </button>
  );
}

function ButtonSecondaryDisabled(props) {
  return (
    <button
      className="inter button-reset ph4 pv3 bg-white black ba b--black bw1 o-50"
      disabled
    >
      {props.children}
    </button>
  );
}

export {
  ButtonPrimary,
  ButtonPrimaryDisabled,
  ButtonSecondary,
  ButtonSecondaryDisabled,
};

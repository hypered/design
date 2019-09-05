import React from "react";

function H1(props) {
  return <h1 class="f1 lh-title mv2">{props.children}</h1>;
}

function H2(props) {
  return <h2 class="f2 lh-title mv2">{props.children}</h2>;
}

function H3(props) {
  return <h3 class="f3 lh-title mv2">{props.children}</h3>;
}

function H4(props) {
  return <h4 class="f4 lh-title mv2">{props.children}</h4>;
}

function H5(props) {
  return <h5 class="f5 lh-title mv2">{props.children}</h5>;
}

function H6(props) {
  return <h6 class="f6 lh-title mv2 ttu">{props.children}</h6>;
}

export { H1, H2, H3, H4, H5, H6 };

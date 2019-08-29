import React from "react";

function Button(props) {
  return (
    <button className="button-reset ph4 pv3 bg-black white bn" {...props}>
      {props.children}
    </button>
  );
}

export { Button };

import React from "react";

function Input(props) {
  const { message, ...props2 } = props;
  return (
    <div className="mv3">
      <div className="mb3">
        <label className="db fw6 mv1">{props.label}</label>
        <input
          type={props.type}
          className="input-reset bl-0 bt-0 br-0 bb bg-near-white pv3 ph2 w-100 outline-0 border-box"
          value={props.value}
          placeholder={props.placeholder}
          {...props2}
        />
        <div className="mv1 h1 red fw5">{props.message}</div>
      </div>
    </div>
  );
}

export { Input };

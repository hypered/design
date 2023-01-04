import React from "react";

export const ContainerWithLabel = ({ label, children }) => {
  return (
    <div>
      <label className="dib bg-red white f6 mv0 pv1 ph2">{label}</label>
      <div className="ba b--red debug-grid-16">{children}</div>
    </div>
  );
};

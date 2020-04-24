import React from "react";
import { ContainerWithLabel } from "../../components";

export const StatusCode = ({ status, statusCode, children }) => {
  return (
    <ContainerWithLabel label="Status">
      <div className="pa5">
        <h2 className="red f3 fw8 tracked-tight lh-title mv3 ttu">Error</h2>
        <h3
          className="glitch f1 f-headline-m f-headline-l fw9 tracked-tight lh-title mv3"
          data-text={`${statusCode} ${status}`}
        >
          {statusCode} {status}
        </h3>
        <div>{children}</div>
      </div>
    </ContainerWithLabel>
  );
};

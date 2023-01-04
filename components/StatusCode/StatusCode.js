import React from "react";
import { ContainerWithLabel, P, UL, LI, A } from "../../components";

export const StatusCode = ({ status, statusCode, children }) => {
  return (
    <ContainerWithLabel label="Status">
      <div className="pa6">
        <h2 className="red f4 fw8 tracked-tight lh-title mv0 ttu">Error</h2>
        <h3
          className="glitch f1 f-subheadline-m f-subheadline-l fw9 tracked-tight lh-title mv0"
          data-text={`${statusCode} ${status}`}
        >
          {statusCode} {status}
        </h3>
        <div>{children}</div>
      </div>
    </ContainerWithLabel>
  );
};

const Custom404Content = () => {
  return (
    <div>
      <P>
        Looks like the page you're looking for is unavailable. You can click
        here to return to the home page, or visit any of the links below:
      </P>

      <UL>
        <LI>
          <A href="/">Home</A>
        </LI>
        <LI>
          <A href="components/">Components</A>
        </LI>
        <LI>
          <A href="storybook/">Storybook</A>
        </LI>
        <LI>
          <A href="documentation/">Documentation</A>
        </LI>
        <LI>
          <A href="haddock/">Haddock</A>
        </LI>
      </UL>
    </div>
  );
};

export const Custom400 = () => {
  return (
    <StatusCode status="Bad Gateway" statusCode="400">
      <Custom404Content />
    </StatusCode>
  );
};

export const Custom404 = () => {
  return (
    <StatusCode status="Not Found" statusCode="404">
      <Custom404Content />
    </StatusCode>
  );
};

import React from "react";
import { StatusCode, P, UL, LI, A } from "../../components";

export default {
  title: "Status Codes"
};

const NotFoundContent = () => {
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

const Container = ({ children }) => {
  return (
    <div className="flex items-center justify-center vh-100 mw7 center pa4">
      {children}
    </div>
  );
};

export const BadGateway = () => {
  return (
    <Container>
      <StatusCode status="Bad Gateway" statusCode="400">
        <NotFoundContent />
      </StatusCode>
    </Container>
  );
};

export const NotFound = () => {
  return (
    <Container>
      <StatusCode status="Not Found" statusCode="404">
        <NotFoundContent />
      </StatusCode>
    </Container>
  );
};

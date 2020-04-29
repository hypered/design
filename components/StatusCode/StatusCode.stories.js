import React from "react";
import { Custom400, Custom404 } from "../../components";

export default {
  title: "Status Codes"
};

const Container = ({ children }) => {
  return (
    <div className="flex items-center justify-center vh-100 mw8 center pa4">
      {children}
    </div>
  );
};

export const Error400 = () => {
  return (
    <Container>
      <Custom400 />
    </Container>
  );
};

export const Error404 = () => {
  return (
    <Container>
      <Custom404 />
    </Container>
  );
};

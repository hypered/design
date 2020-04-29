import React from "react";

import { HomeLayout, StatusCode } from "../components";

function Error({ statusCode }) {
  return (
    <HomeLayout>
      <div className="mw8 center">
        <StatusCode status="Bad Gateway" statusCode="400"></StatusCode>
      </div>
    </HomeLayout>
  );
}

Error.getInitialProps = ({ res, err }) => {
  const statusCode = res ? res.statusCode : err ? err.statusCode : 404;
  return { statusCode };
};

export default Error;

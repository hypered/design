import React from "react";

import { HomeLayout, StatusCode } from "../components";

function Error({ statusCode }) {
  return (
    <HomeLayout>
      <div className="mw8 center">
        <StatusCode status="Error" statusCode={statusCode}>
          {statusCode
            ? `An error ${statusCode} occured on server`
            : `An error occured on client`}
        </StatusCode>
      </div>
    </HomeLayout>
  );
}

Error.getInitialProps = ({ res, err }) => {
  const statusCode = res ? res.statusCode : err ? err.statusCode : 404;
  return { statusCode };
};

export default Error;

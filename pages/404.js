import React from "react";

import { HomeLayout, Custom404 } from "../components";

{
  /*
  this file is statically generated at build time:
  https://nextjs.org/docs/advanced-features/custom-error-page#404-page
  */
}

function Error404() {
  return (
    <HomeLayout>
      <div className="mw8 center">
        <Custom404 status="Not Found" statusCode="404" />
      </div>
    </HomeLayout>
  );
}

export default Error404;

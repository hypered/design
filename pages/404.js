import React from "react";

import { HomeLayout, HeaderSection, StatusCode } from "../components";

function Home() {
  return (
    <HomeLayout>
      <div className="mw7 center">
        <StatusCode status="Not Found" statusCode="404">
          testing
        </StatusCode>
      </div>
    </HomeLayout>
  );
}

export default Home;

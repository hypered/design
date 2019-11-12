import React from "react";

import {
  HomeLayout,
  H1,
  P,
  ComponentsSection,
  FooterSection,
} from "../components";

function Components() {
  return (
    <HomeLayout>
      <header className="pv4 pv5-l">
        <H1>Components</H1>
        <P>hello world</P>
      </header>

      <ComponentsSection />
    </HomeLayout>
  );
}

export default Components;

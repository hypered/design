import React from "react";

import {
  HeaderSection,
  IntroductionSection,
  ComponentsSection,
} from "../components";

function Home() {
  return (
    <div className="mw8 center ph4">
      <HeaderSection />

      <IntroductionSection />

      <ComponentsSection />
    </div>
  );
}

export default Home;

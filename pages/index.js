import React from "react";

import {
  HeaderSection,
  IntroductionSection,
  ComponentsSection,
  FooterSection,
} from "../components";

function Home() {
  return (
    <div className="mw8 center ph4">
      <HeaderSection />

      <IntroductionSection />

      <ComponentsSection />

      <FooterSection />
    </div>
  );
}

export default Home;

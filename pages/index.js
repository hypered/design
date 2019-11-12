import React from "react";

import {
  HeaderSection,
  IntroductionSection,
  ComponentsSection,
  FooterSection,
} from "../components";

function Home() {
  return (
    <div className="mw8 center ph3 ph4-m ph4-l">
      <HeaderSection />

      <IntroductionSection />

      <ComponentsSection />

      <FooterSection />
    </div>
  );
}

export default Home;

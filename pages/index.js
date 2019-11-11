import React from "react";

import {
  HeaderSection,
  IntroductionSection,
  ComponentsSection,
} from "../components";

function Home() {
  return (
    <main className="mw8 center ph4">
      <HeaderSection />

      <IntroductionSection />

      <ComponentsSection />
    </main>
  );
}

export default Home;

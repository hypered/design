import React from "react";

import {
  HomeLayout,
  HeaderSection,
  IntroductionSection,
  ComponentsSection,
} from "../components";

function Home() {
  return (
    <HomeLayout>
      <HeaderSection />

      <IntroductionSection />

      <ComponentsSection />
    </HomeLayout>
  );
}

export default Home;

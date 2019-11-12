import React from "react";

import {
  HomeLayout,
  HeaderSection,
  IntroductionSection,
  ComponentsSection,
  FooterSection,
} from "../components";

function Home() {
  return (
    <HomeLayout>
      <HeaderSection />

      <IntroductionSection />

      <ComponentsSection />

      <FooterSection />
    </HomeLayout>
  );
}

export default Home;

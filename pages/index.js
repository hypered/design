import React from "react";

import {
  HomeLayout,
  HeaderSection,
  GettingStartedSection,
  ComponentsSection,
} from "../components";

function Home() {
  return (
    <HomeLayout>
      <HeaderSection />

      <GettingStartedSection />

      <ComponentsSection />
    </HomeLayout>
  );
}

export default Home;

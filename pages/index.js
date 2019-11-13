import React from "react";

import {
  HomeLayout,
  HeaderSection,
  GettingStartedSection,
  GalleryPreviewSection,
} from "../components";

function Home() {
  return (
    <HomeLayout>
      <HeaderSection />

      <GettingStartedSection />

      <GalleryPreviewSection />
    </HomeLayout>
  );
}

export default Home;

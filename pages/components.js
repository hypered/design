import React from "react";

import {
  HomeLayout,
  Divider,
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
        <P>
          Mauris ornare sit amet felis non lacinia. Fusce a tristique purus.
          Donec sodales sapien ac risus eleifend congue. Nulla suscipit massa at
          purus sagittis venenatis. Sed iaculis vestibulum justo id accumsan.
          Morbi finibus gravida pharetra. Duis id venenatis velit, auctor
          sodales nunc.
        </P>
      </header>

      <Divider />

      <ComponentsSection />
    </HomeLayout>
  );
}

export default Components;

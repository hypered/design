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

      <section>
        <div className="flex flex-wrap nl3 nr3">
          <div className="w-80 ph3">
            <ComponentsSection />
          </div>
        </div>
      </section>

      <FooterSection />
    </HomeLayout>
  );
}

export default Components;

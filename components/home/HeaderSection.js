import React from "react";

import {
  Divider,
  ButtonLinkPrimaryPill,
  ButtonLinkSecondaryPill,
} from "../../components";

export const HeaderSection = props => (
  <>
    <header className="pv4 pv5-l">
      <h1 className="f2 f-subheadline-l fw6 fw5-m fw5-l tracked-tight mv0 lh-title relative">
        Hypered <span className="fw3">Design System</span>
      </h1>
      {/* @TODO: replace copy */}
      <p className="f5 fw5 measure-wide lh-copy">
        Lorem ipsum dolor sit amet, consectetur adipiscing elit. Praesent
        convallis mollis nulla, molestie tempor velit consequat non.
      </p>

      <div className="flex flex-wrap nl2 nr2 mt4">
        <div className="w-100 w-auto-m w-auto-l ph2 mb3 mb0-m mb0-l">
          <a
            className="no-underline db tc ph4 pv3 bg-black white ba bw1 b--black br-pill"
            href="components/"
          >
            <b>Browse Components</b>
          </a>
        </div>

        <div className="w-100 w-auto-m w-auto-l ph2">
          <a
            className="no-underline db tc ph4 pv3 bg-white black ba b--black bw1 br-pill"
            href="https://github.com/hypered/design"
          >
            Contribute on GitHub
          </a>
        </div>
      </div>
    </header>

    <Divider />
  </>
);

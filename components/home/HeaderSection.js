import React from 'react'

import {
  Divider,
  ButtonLinkPrimaryPill,
  ButtonLinkSecondaryPill,
} from "../../components";

export const HeaderSection = props => (
  <>
    <header className="pv5">
      <h2 className="f4 mv0 lh-title relative top-1">Hypered</h2>
      <h1
        className="f-subheadline-m f-headline-l fw5 tracked-tight mv0 lh-title"
        style={{ position: "relative", left: -6 }}
      >
        Design System
      </h1>
      {/* @TODO: replace copy */}
      <p className="f5 fw5 measure-wide lh-copy">
        Lorem ipsum dolor sit amet, consectetur adipiscing elit. Praesent
        convallis mollis nulla, molestie tempor velit consequat non. Integer
        quam ligula, consequat eget semper in, sodales nec mauris. Sed ultrices
        enim quis eros lobortis, semper condimentum eros sodales. Morbi iaculis
        lectus id dui convallis feugiat.
      </p>

      <div className="flex nl2 nr2 mt4">
        <div className="ph2">
          <ButtonLinkPrimaryPill href="#">
            <b>Browse Components</b>
          </ButtonLinkPrimaryPill>
        </div>
        <div className="ph2">
          <ButtonLinkSecondaryPill href="#">
            Contribute on GitHub
          </ButtonLinkSecondaryPill>
        </div>
      </div>
    </header>

    <Divider />
  </>
);

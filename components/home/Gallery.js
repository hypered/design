import React from "react";
import styled from "styled-components";

const OverlayStyled = styled.a.attrs(props => ({
  className: "no-underline absolute absolute--fill bg-black-10",
}))`
  opacity: 0;
`;

const Overlay = props => (
  <OverlayStyled href="#">
    <div className="absolute right-0 bottom-0 inline-flex bg-black white f6 fw6 pv2 ph3">
      View Storybook &#8599;
    </div>
  </OverlayStyled>
);

const AspectRatioObject = styled.div.attrs(props => ({
  className:
    "aspect-ratio--object pa4 flex items-center justify-center overflow-hidden",
}))`
  &:hover > ${OverlayStyled} {
    opacity: 1;
  }
`;

export const Gallery = props => (
  <div className="db black no-underline w-100 w-50-m w-50-l ph3 mb4" href="#">
    <div className="bg-white ba b--black aspect-ratio aspect-ratio--7x5 relative z-0">
      <AspectRatioObject>
        <Overlay />
        {props.children}
      </AspectRatioObject>
    </div>

    <div className="f5 fw5 tc pt3">{props.title}</div>
  </div>
);

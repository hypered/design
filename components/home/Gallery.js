import React from "react";
import styled from "styled-components";

const OverlayStyled = styled.div.attrs(props => ({
  className:
    "absolute absolute--fill yellow bg-black-30 flex justify-center items-center",
}))`
  opacity: 0;
`;

const Overlay = props => (
  <OverlayStyled>
    <div className="inline-flex bg-black fw6 pa3">View Storybook &#8599;</div>
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
  <a className="db black no-underline w-100 w-50-m w-50-l ph3 mb4" href="#">
    <div className="bg-light-gray aspect-ratio aspect-ratio--7x5 relative z-0">
      <AspectRatioObject>
        <Overlay />
        {props.children}
      </AspectRatioObject>
    </div>

    <div className="f5 fw5 tc pt3">{props.title}</div>
  </a>
);

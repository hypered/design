import React from "react";
import styled from "styled-components";

const OverlayStyled = styled.a.attrs(props => ({
  className: "no-underline absolute absolute--fill",
}))`
  opacity: 0;
`;

const Overlay = props => (
  <OverlayStyled href={props.href}>
    {props.href ? (
      <div className="absolute right-0 bottom-0 inline-flex bg-black white f6 fw6 pv2 ph3">
        View Storybook &#8599;
      </div>
    ) : null}
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
  <div className="db black no-underline w-100 w-50-m w-50-l ph3 mb4">
    <div className="bg-white ba b--black aspect-ratio aspect-ratio--7x5 relative z-0">
      <AspectRatioObject>
        <Overlay href={props.href} />
        {props.children}
      </AspectRatioObject>
    </div>

    <div className="f5 fw5 tc pt3">{props.title}</div>
  </div>
);

import React from "react";
import { BlockQuote, PullQuote } from "../../components";

export default {
  title: "Block Quote",
};

export const Default = () => (
  <BlockQuote>
    You have power over your mind - not outside events. Realize this, and you
    will find strength.
  </BlockQuote>
);

export const PullQuoteExample = () => (
  <PullQuote>
    Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam consectetur
    tincidunt elit, et semper enim laoreet eu. In hac habitasse platea dictumst.
    Phasellus consequat quis augue vitae laoreet. In consequat, urna vel
    volutpat dignissim, eros eros sodales quam, a suscipit felis eros non dolor.
  </PullQuote>
);

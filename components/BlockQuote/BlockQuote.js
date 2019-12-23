import React from "react";
import cx from "classnames";

export const BlockQuote = props => (
  <blockquote className="db bl bw2 pv2 ph3 ml0 mv4 lh-copy">
    <span className="i">{props.children}</span>
  </blockquote>
);

export const PullQuote = props => {
  let pullQuoteClassNames = cx(
    {
      "pull-quote": !props.hideQuote,
    },
    "relative",
    "db",
    "pv3",
    "ph4",
    "f4",
    "ml0",
    "mv4",
    "lh-copy",
  );
  return (
    <blockquote className={pullQuoteClassNames}>
      <span className="i">{props.children}</span>
    </blockquote>
  );
};

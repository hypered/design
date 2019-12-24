import React from "react";
import cx from "classnames";

export const Image = ({ pull, fullWidth, src, caption }) => {
  let figureClassNames = cx("mv0", {
    nl4: pull && !fullWidth,
    nr4: pull && !fullWidth,
    "w-100": fullWidth,
    mh0: fullWidth,
  });

  return (
    <figure className={figureClassNames}>
      <img className="img v-top" src={src} />
      {caption && <figcaption className="f6 gray mv1 tc">{caption}</figcaption>}
    </figure>
  );
};

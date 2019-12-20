import React from "react";
import cx from "classnames";

export const TitleJumbo = ({ children }) => (
  <h1 className="f1 f-subheadline-m f-subheadline-l tracked-tight mv2">
    {children}
  </h1>
);

export const SubTitleJumbo = ({ children }) => (
  <h2 className="f1 tracked-tight mv2">{children}</h2>
);

export const Title = ({ children }) => (
  <h1 className="f1 f1-l tracked-tight mv2">{children}</h1>
);

export const SubTitle = ({ children }) => (
  <h2 className="f2 tracked-tight mv2">{children}</h2>
);

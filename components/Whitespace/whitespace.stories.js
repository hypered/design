import React from "react";
import { Layout } from "../../components";

export default {
  title: "Whitespace",
};

export const AutoWidth = () => {
  return (
    <div className="flex justify-center">
      <div className="ba b--black h5 pa3 w-auto self-center">
        This box takes up the size of its content and is centered.
      </div>
    </div>
  );
};

export const NegativeMargins = () => {
  return (
    <div className="nl4 nr4">
      <div className="ba b--black w-100 h5 pa3">
        This box has negative margins and should extend beyond its parent's
        container.
      </div>
    </div>
  );
};

export const FullWidth = () => {
  return (
    <div className="w-100">
      <div className="ba b--black w-100 h5 pa3">
        This box takes up the full width of its parent's container.
      </div>
    </div>
  );
};

export const Examples = () => {
  return (
    <Layout>
      <AutoWidth />
      <NegativeMargins />
      <FullWidth />
    </Layout>
  );
};

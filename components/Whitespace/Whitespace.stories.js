import React from "react";
import { Layout } from "../../components";

export default {
  title: "Whitespace",
};

const Box = ({ title, description }) => {
  return (
    <div className="bg-navy light-green flex justify-between h5">
      <div className="flex flex-column justify-between pa4">
        <h2 className="f3 f2-m f2-l fw8 lh-title mv1">{title}</h2>
        <div>
          <p className="f7 b tracked lh-copy ttu mv1 o-50">Description</p>
          <p className="f5 lh-copy mv1">{description}</p>
        </div>
      </div>
      <b
        className="flex items-center justify-start pa3 bl f6"
        style={{ writingMode: "vertical-lr", textOrientation: "mixed" }}
      >
        Hypered Design System
      </b>
    </div>
  );
};

export const AutoWidth = () => {
  return (
    <div className="flex justify-center mb4">
      <div className="h5 w-auto self-center">
        <Box
          title="01. Auto Width"
          description="This box takes up the size of its content and is centered."
        />
      </div>
    </div>
  );
};

export const NegativeMargins = () => {
  return (
    <div className="nl4 nr4 mb4">
      <Box
        title="02. Negative Margins"
        description="This box has negative margins and should extend beyond its parent's
        container."
      />
    </div>
  );
};

export const FullWidth = () => {
  return (
    <div className="w-100">
      <Box
        title="03. Full Width"
        description="This box takes up the full width of its parent's container."
      />
    </div>
  );
};

export const Examples = () => {
  return (
    <Layout>
      <h1 className="dib bg-red white f6 mv0 pv1 ph2">Container</h1>
      <div className="ba b--red debug-grid-16">
        <AutoWidth />
        <NegativeMargins />
        <FullWidth />
      </div>
    </Layout>
  );
};

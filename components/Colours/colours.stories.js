import React from "react";
import cx from "classnames";

export default {
  title: "Colours",
};

const TextRow = ({ className, label }) => {
  let textRowClassNames = cx(className, "mb3");

  return <div className={textRowClassNames}>Hypered</div>;
};

const BGRow = ({ className, label }) => {
  let bgRowClassNames = cx(className, "w2", "h2", "mr2");

  return (
    <div className="flex items-center mb3">
      <div className={bgRowClassNames}></div>
      <div>{label}</div>
    </div>
  );
};

const Specimen = ({ className }) => {
  let specimenClassName = cx(
    className,
    "aspect-ratio",
    "aspect-ratio--4x3",
    "relative",
  );

  return (
    <div className="w-100 w-50-l ph2 mb3">
      <div className={specimenClassName}>
        <div className="aspect-ratio--object flex items-stretch">
          <div className="flex flex-column justify-between pa4 h-100">
            <h2 className="f2 f1-m f1-l mv2 fw6 tracked-tight lh-title">
              Samples <span className="o-50">↗</span>
            </h2>
            <p className="f6 f5-m f5-l lh-copy">
              The user interface (UI), in the industrial design field of
              human-computer interaction, is the space where interactions
              between humans and machines occur.
            </p>
          </div>
          <div className="bl w4">
            <div
              className="flex justify-between items-center pa3 h-100"
              style={{ writingMode: "vertical-lr", textOrientation: "mixed" }}
            >
              <div className="b">Hypered Design System</div>
              <div>Volume 001</div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export const Text = () => (
  <>
    <TextRow className="hy-blue" />
    <TextRow className="hy-red" />
    <TextRow className="hy-green" />
    <TextRow className="hy-yellow" />
  </>
);

export const Background = () => (
  <>
    <BGRow className="hy-bg-blue" label="Blue" />
    <BGRow className="hy-bg-red" label="Red" />
    <BGRow className="hy-bg-green" label="Green" />
    <BGRow className="hy-bg-yellow" label="Yellow" />
  </>
);

export const Samples = () => (
  <div className="pa4">
    <div className="flex flex-wrap nl2 nr2 mw8">
      <Specimen className="hy-bg-blue light-green" />
      <Specimen className="hy-bg-red light-yellow" />
      <Specimen className="hy-bg-green black" />
      <Specimen className="hy-bg-yellow hy-red" />

      <Specimen className="hy-bg-blue white" />
      <Specimen className="hy-bg-red white" />
      <Specimen className="hy-bg-green white" />
      <Specimen className="hy-bg-yellow white" />
    </div>
  </div>
);

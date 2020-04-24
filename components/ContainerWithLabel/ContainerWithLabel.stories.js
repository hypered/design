import React from "react";
import { ContainerWithLabel } from "../../components";

export default {
  title: "Container With Label"
};

export const Default = () => {
  return (
    <div className="pa6 mw7 center">
      <ContainerWithLabel label="Warning!">
        <div className="aspect-ratio aspect-ratio--3x4 relative">
          <div className="aspect-ratio--object pa4">
            <h2 className="f-headline fw9 tracked-tight lh-title mv3">
              Error 50x
            </h2>
            <p className="f5 lh-copy mv3">
              Looks like the page that you're looking for is not available.
              Please click here to return to the home page.
            </p>
          </div>
        </div>
      </ContainerWithLabel>
    </div>
  );
};

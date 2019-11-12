import React from "react";
import { Divider } from "../../components";

export const Section = props => (
  <>
    <section className="pv4 pv5-l">{props.children}</section>
    <Divider />
  </>
);

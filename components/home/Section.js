import React from "react";
import { Divider } from "../../components";

export const Section = props => (
  <>
    <section className="pv5">{props.children}</section>
    <Divider />
  </>
);

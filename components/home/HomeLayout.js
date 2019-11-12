import React from "react";
import { FooterSection } from "../../components";

export const HomeLayout = props => (
  <div className="mw8 center ph3 ph4-m ph4-l">
    {props.children}
    <FooterSection />
  </div>
);

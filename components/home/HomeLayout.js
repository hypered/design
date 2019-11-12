import React from "react";
import { NavSection, FooterSection } from "../../components";

export const HomeLayout = props => (
  <div className="mw8 center ph3 ph4-m ph4-l">
    <NavSection />
    {props.children}
    <FooterSection />
  </div>
);

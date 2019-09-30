import React from "react";

import { Input } from "../../components";

export default {
  title: "Input",
};

export const text = () => (
  <Input type="text" label="Full Name" placeholder="John Appleseed" />
);

export const password = () => (
  <Input type="password" label="Password" value="hello, world" />
);

export const number = () => <Input type="number" label="Quantity" value="20" />;

export const withMessage = () => (
  <Input
    type="email"
    label="Email Address"
    value="john.appleseed@gma"
    message="Not a valid email."
  />
);

export const usageExample = () => (
  <div className="mw6 pa4">
    <Input type="text" label="Full Name" placeholder="John Appleseed" />
    <Input
      type="email"
      label="Email Address"
      value="john.appleseed@gma"
      message="Not a valid email."
    />
    <Input type="password" label="Password" value="hello, world" />
  </div>
);

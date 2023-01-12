import { Input } from "../../components";

export default {
  title: "Input",
};

export const Text = () => (
  <Input type="text" label="Full Name" placeholder="John Appleseed" />
);

export const Password = () => (
  <Input type="password" label="Password" value="hello, world" />
);

export const Number = () => (
  <Input type="number" label="Quantity" value="20" />
);

export const WithMessage = () => (
  <Input
    type="email"
    label="Email Address"
    value="john.appleseed@example.com"
    message="Not a valid email."
  />
);

export const Usage = () => (
  <div className="mw6 pa4">
    <Input type="text" label="Full Name" placeholder="John Appleseed" />
    <Input
      type="email"
      label="Email Address"
      value="john.appleseed@example.com"
      message="Not a valid email."
    />
    <Input type="password" label="Password" value="hello, world" />
  </div>
);

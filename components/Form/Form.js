import { Input, Button } from "../../components";

export const LoginForm = () => (
  <form className="pa4 bg-white ba b--black bw1 mw6">
    <h2>Log in to your account</h2>
    <Input
      type="email"
      label="Email"
      placeholder="john@doe.com"
      message="You have entered an invalid email"
    />
    <Input type="password" label="Password" placeholder="" />
    <div className="flex flex-wrap justify-between">
      <a className="link no-underline black hover-blue self-center" href="#">
        Forgot Password
      </a>
      <Button variant="primary">Log In —></Button>
    </div>
  </form>
);

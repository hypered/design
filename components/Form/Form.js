import { Input, Button } from "../../components";

export const LoginForm = () => (
  <form className="bg-white mw6">
    <div className="pa4 bt br bl b--black bw1">
      <h2>Log in to your account</h2>
      <Input
        type="email"
        label="Email"
        placeholder="john@doe.com"
        message="You have entered an invalid email"
      />
      <Input type="password" label="Password" placeholder="" />
      <a className="link no-underline black hover-blue" href="#">
        Forgot Password
      </a>
    </div>
    <div className="flex justify-between">
      <Button variant="secondary" size="large">
        Sign Up
      </Button>
      <Button variant="primary" size="large">
        Log In —>
      </Button>
    </div>
  </form>
);

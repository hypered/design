import { Input, Button } from "../../components";

export const LoginForm = () => (
  <form className="bg-white ba b--black bw1 mw6">
    <div className="pa4">
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
      <Button variant="primary" size="large">
        Log In —>
      </Button>
    </div>
  </form>
);

import { Input, Button, A } from "../../components";

export const LoginContent = () => (
  <>
    <Input
      type="email"
      label="Email"
      placeholder="john@doe.com"
      message="You have entered an invalid email"
    />
    <Input type="password" label="Password" placeholder="" />
    <A color="black" href="#">
      Forgot Password
    </A>
  </>
);

export const LoginForm = () => (
  <form className="bg-white mw6" onSubmit={e => e.preventDefault()}>
    <div className="pa4 bt br bl b--black bw1">
      <h2>Log in to your account</h2>
      <LoginContent />
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

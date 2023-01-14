import { Input, Button, A } from "../../components";

export const LoginContent = () => (
  <>
    <Input
      label="Username"
      type="text"
      name="username"
      id="username"
      message="You have entered an invalid email"
    />
    <Input
      label="Password"
      type="password"
      placeholder=""
      name="password"
      id="password"
    />
    <A color="black" href="#">
      Reset password
    </A>
  </>
);

export const LoginForm = () => (
  <form className="bg-white mw6" method="POST" action="/echo/login">
    <div className="pa4 bt br bl b--black bw1">
      <h2>Log in to Appname</h2>
      <LoginContent />
    </div>
    <div className="flex justify-between">
      <Button variant="secondary" size="large">
        Register
      </Button>
      <Button variant="primary" size="large">
        Log in —>
      </Button>
    </div>
  </form>
);

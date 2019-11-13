import { Nav, NavLink } from "../../components";

export default {
  title: "Navigation",
};

export const Navigation = () => (
  <Nav>
    <NavLink href="#" active={true}>
      Item 1
    </NavLink>
    <NavLink href="#">Item 2</NavLink>
    <NavLink href="#">Item 3</NavLink>
  </Nav>
);

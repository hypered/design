import { Nav, NavLink } from "../../components";

export default {
  title: "Navigation",
};

export const Navigation = () => (
  <Nav>
    <div>
      <NavLink href="#" active={true}>
        noteed.com
      </NavLink>
      <NavLink href="#">blog</NavLink>
      <NavLink href="#">not-os</NavLink>
    </div>
  </Nav>
);

export const NavigationSpaceBetween = () => (
  <Nav>
    <div>
      <NavLink href="#">noteed.com</NavLink>
    </div>
    <div>
      <NavLink href="#">blog</NavLink>
      <NavLink href="#">not-os</NavLink>
    </div>
  </Nav>
);

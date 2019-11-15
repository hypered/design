export const NavLink = React.forwardRef(function NavLink(props, ref) {
  return (
    <a
      className={`link mr3 black hover-blue${props.active ? " fw7" : " fw5"}`}
      {...props}
    >
      {props.children}
    </a>
  );
});

export const Nav = props => (
  <nav className="flex align-items-center lh-copy mb4 pv3">
    {props.children}
  </nav>
);

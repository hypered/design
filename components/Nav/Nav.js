import cx from "classnames";

export const NavLink = React.forwardRef(function NavLink(props, ref) {
  let NavLinkClasses = cx("link", "black", "hover-blue", {
    mr3: !props.lastItem,
  });

  return (
    <a className={NavLinkClasses} {...props}>
      {props.children}
    </a>
  );
});

export const Nav = props => (
  <nav className="flex justify-between align-items-center lh-copy mb4 pv3">
    {props.children}
  </nav>
);

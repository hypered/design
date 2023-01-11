import cx from "classnames";

export const NavLink = React.forwardRef(function NavLink(props, ref) {
  let NavLinkClasses = cx("black", "hy-hover-blue", {
    underline: props.active,
    mr3: !props.lastItem,
  });

  const { active, ...props2 } = props;
  return (
    <a className={NavLinkClasses} {...props2}>
      {props.children}
    </a>
  );
});

export const Nav = props => (
  <nav className="flex justify-between align-items-center lh-copy mb4 pv3">
    {props.children}
  </nav>
);

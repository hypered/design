import { HR } from "../../components";

export const Footer = (props) => (
  <footer>
    <HR />
    <p className="inline-flex lh-copy">
      &copy; {props.copyright ? props.copyright : "Hypered, 2019-2023."}
    </p>
  </footer>
);

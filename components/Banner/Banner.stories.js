import { Banner } from "../../components";

export default {
  title: "Banner",
};

export const Green = () => (
  <Banner color="green">Messages sent!</Banner>
);

export const Red = () => (
  <Banner color="red">Error, something is wrong.</Banner>
);

export const Yellow = () => (
  <Banner color="yellow">Something might be wrong.</Banner>
);

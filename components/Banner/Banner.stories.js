import { BannerGreen, BannerYellow, BannerRed } from "../../components";

export default {
  title: "Banner",
};

export const Green = () => <BannerGreen>Messages sent!</BannerGreen>;

export const Yellow = () => (
  <BannerYellow>Something might be wrong.</BannerYellow>
);

export const Red = () => <BannerRed>Error, something is wrong.</BannerRed>;

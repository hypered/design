import { OL, UL, LI } from "../../components";

export default {
  title: "List",
};

export const OrderedList = () => (
  <OL>
    <LI>Apple</LI>
    <LI>Banana</LI>
    <LI>Cherry</LI>
    <LI>Durian</LI>
  </OL>
);

export const UnorderedList = () => (
  <UL>
    <LI>Apple</LI>
    <LI>Banana</LI>
    <LI>Cherry</LI>
    <LI>Durian</LI>
  </UL>
);

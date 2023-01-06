import { H1, H2, H3, H4, H5, H6, P, A } from "../../components";

export default {
  title: "Typography",
};

export const Heading1 = () => <H1>Heading 1</H1>;

export const Heading2 = () => <H2>Heading 2</H2>;

export const Heading3 = () => <H3>Heading 3</H3>;

export const Heading4 = () => <H4>Heading 4</H4>;

export const Heading5 = () => <H5>Heading 5</H5>;

export const Heading6 = () => <H6>Heading 6</H6>;

export const Paragraph = () => (
  <P>
    Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nulla sollicitudin
    malesuada est. Sed efficitur laoreet massa, eu dictum est luctus sit amet.
    Morbi elementum dapibus pellentesque. Sed varius nisi nisi, nec imperdiet
    nunc bibendum at. Maecenas bibendum, neque nec vehicula dignissim, sem dolor
    congue risus, ac consequat sapien nibh in felis. Suspendisse at est a nisl
    dictum condimentum. Suspendisse ut dolor vitae nisi dictum hendrerit a vel
    magna. Etiam porttitor lacus magna, non bibendum tellus lobortis sit amet.
    Duis quis lectus massa. Nam quis fringilla dui. Fusce felis leo, iaculis id
    dui eu, lacinia varius dolor. Praesent molestie rhoncus mi, ac malesuada
    neque placerat vitae. Aliquam placerat auctor pretium. Mauris egestas
    condimentum erat sit amet tempor. Quisque imperdiet, augue nec eleifend
    placerat, lacus tortor pellentesque metus, sit amet laoreet lectus enim et
    mauris. Vivamus sollicitudin a ex sit amet ullamcorper.
  </P>
);

export const UsageExample = () => (
  <article className="measure-wide">
    <H1>Design System Blog</H1>
    <P>This is an intro to using Hypered design system.</P>
    <H2>Components</H2>
    <P>
      Hypered design system comprises of components that quickly help you get
      started with your projects.
    </P>
    <P>
      The components in this design system are built with{" "}
      <A color="blue" href="#">
        Tachyons
      </A>
      .
    </P>
  </article>
);

import React from "react";
import { Layout, H1, H2, P, Image } from "../../components";

export default {
  title: "Images",
};

export const Default = () => <Image src="img/placeholder.svg" />;

export const NegativePull = () => <Image src="img/placeholder.svg" pull />;

export const FullWidth = () => <Image src="img/placeholder.svg" fullWidth />;

export const WithCaption = () => (
  <Image src="img/placeholder.svg" caption="This is a placeholder image." />
);

export const Examples = () => (
  <Layout>
    <main className="debug">
      <article className="mw7 cf">
        <H1>Using the Image component</H1>
        <P>
          In this example we will be exploring different ways of using the Image
          component. There are three variations of the Image component namely;{" "}
          <i>default</i>, <i>negative pull</i>, and, <i>full-width</i>. There is
          also an optional option should you need to add one.
        </P>

        <H2>Default</H2>

        <Image src="img/placeholder.svg" />

        <H2>With Caption</H2>

        <Image
          src="img/placeholder.svg"
          caption="This is an image with a caption."
        />

        <H2>With Negative Pull</H2>

        <Image
          src="img/placeholder.svg"
          pull
          caption="This is an image with negative margins applied to it."
        />

        <H2>Full Width Image</H2>

        <Image
          src="img/placeholder.svg"
          fullWidth
          caption="This is an image that takes up the whole width of its parent."
        />
      </article>
    </main>
  </Layout>
);

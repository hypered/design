import { Layout } from "../../components";

export default {
  title: "Layout",
};

export const Default = () => <Layout>Content goes here</Layout>;

export const BlogList = () => (
  <BlogListLayout>Content goes here</BlogListLayout>
);

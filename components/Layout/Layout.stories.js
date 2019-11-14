import { Layout, BlogListLayout, BlogPostLayout } from "../../components";

export default {
  title: "Layouts",
};

export const Default = () => <Layout>Content goes here</Layout>;

export const BlogList = () => <BlogListLayout />;

export const BlogPost = () => <BlogPostLayout />;

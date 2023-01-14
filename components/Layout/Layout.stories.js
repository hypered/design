import {
  Layout,
  BlogListLayout,
  BlogPost1Layout,
  BlogPost2Layout,
  WithSidebarLayout,
} from "../../components";

export default {
  title: "Layouts",
};

export const Default = () => <Layout>Content goes here</Layout>;

export const BlogList = () => <BlogListLayout />;

export const BlogPost1 = () => <BlogPost1Layout />;

export const BlogPost2 = () => <BlogPost2Layout />;

export const WithSidebar = () => <WithSidebarLayout />;

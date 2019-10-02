import { Code, CodeToolbar } from "../../components";

export default {
  title: "Code Block",
};

export const CodeBlock = () => (
  <Code>{`// this is a comment
// this is another comment
// this is a slightly longer comment
`}</Code>
);

export const Editable = () => (
  <Code editable>{`// This block of code can be edited.

// this is a comment
// this is another comment
// this is a slightly longer comment
`}</Code>
);

export const WithToolbar = () => (
  <form>
    <CodeToolbar />
    <Code editable>{`// This block of code can be edited.

// this is a comment
// this is another comment
// this is a slightly longer comment
`}</Code>
  </form>
);

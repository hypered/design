import {
  EditableForm,
  EditablePre,
  EditableTextArea,
  Code,
  CodeToolbar
} from "../../components";

export default {
  title: "Code Block"
};

export const CodeBlock = () => (
  <Code>{`// this is a comment
// this is another comment
// this is a slightly longer comment
`}</Code>
);

export const WithTable = () => (
  <Code>{`
// table examples from: https://ozh.github.io/ascii-tables/

// example 1:
┌──────────────────────────────────┬─────────┬────────────────────────┬────────────────┐
│               Col1               │  Col2   │          Col3          │ Numeric Column │
├──────────────────────────────────┼─────────┼────────────────────────┼────────────────┤
│ Value 1                          │ Value 2 │ 123                    │           10.0 │
│ Separate                         │ cols    │ with a tab or 4 spaces │       -2,027.1 │
│ This is a row with only one cell │         │                        │                │
└──────────────────────────────────┴─────────┴────────────────────────┴────────────────┘

// example 2:

|               Col1               |  Col2   |          Col3          | Numeric Column |
|----------------------------------|---------|------------------------|----------------|
| Value 1                          | Value 2 | 123                    |           10.0 |
| Separate                         | cols    | with a tab or 4 spaces |       -2,027.1 |
| This is a row with only one cell |         |                        |                |
`}</Code>
);

export const Editable = () => (
  <Code editable>{`// This block of code can be edited.

// this is a comment
// this is another comment
// this is a slightly longer comment
`}</Code>
);

export const TextArea = () => (
  <EditableTextArea>{`// This is an example of a block of code that can be edited.

// this is a comment
// this is another comment
// this is a slightly longer comment
`}</EditableTextArea>
);

export const ContentEditableWithToolbar = () => (
  <EditableForm>
    <CodeToolbar title="show fetchgit:README.md" label="Save" />
    <Code editable>{`// This block of code can be edited.

// this is a comment
// this is another comment
// this is a slightly longer comment
`}</Code>
  </EditableForm>
);

export const TextAreaWithToolbar = () => (
  <EditableForm>
    <CodeToolbar title="show fetchgit:README.md" label="Save" />
    <EditableTextArea>{`// This block of code can be edited.

// this is a comment
// this is another comment
// this is a slightly longer comment
`}</EditableTextArea>
  </EditableForm>
);

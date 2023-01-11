import {
  EditableForm,
  EditablePre,
  EditableTextArea,
  Code,
  CodeToolbar,
  CodeToolbarWithButton
} from "../../components";

export default {
  title: "Code Block"
};

const Button = () => (
  <div className="bt pt3 mh3">
    <input
      type="submit"
      className="db button-reset bg-black pa3 white br2 bn"
      value="Submit"
      onClick={e => e.preventDefault()}
    />
  </div>
);

export const Default = () => (
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

export const ContentEditableWithBottomButton = () => (
  <EditableForm>
    <CodeToolbar title="show fetchgit:README.md" />
    <Code editable>{`// This block of code can be edited.

// this is a comment
// this is another comment
// this is a slightly longer comment
`}</Code>
    <Button />
  </EditableForm>
);

export const ContentEditableWithToolbarButton = () => (
  <EditableForm>
    <CodeToolbarWithButton title="show fetchgit:README.md" label="Save" />
    <Code editable>{`// This block of code can be edited.

// this is a comment
// this is another comment
// this is a slightly longer comment
`}</Code>
  </EditableForm>
);

export const TextAreaWithBottomButton = () => (
  <EditableForm>
    <CodeToolbar title="show fetchgit:README.md" label="Save" />
    <EditableTextArea>{`// This block of code can be edited.

// this is a comment
// this is another comment
// this is a slightly longer comment
`}</EditableTextArea>
    <Button />
  </EditableForm>
);

export const TextAreaWithToolbarButton = () => (
  <EditableForm>
    <CodeToolbarWithButton title="show fetchgit:README.md" label="Save" />
    <EditableTextArea>{`// This block of code can be edited.

// this is a comment
// this is another comment
// this is a slightly longer comment
`}</EditableTextArea>
  </EditableForm>
);

import React from "react";
import {
  EditableContainer,
  EditableToolbar,
  ContentEditableBlock,
  ContentEditableForm,
  TextEditableForm
} from "../../components";

export default {
  title: "Editable"
};

const Container = ({ children }) => <div className="mw7 pa4">{children}</div>;

export const ContentEditable = () => (
  <Container>
    <ContentEditableForm />
  </Container>
);

export const TextEditable = () => (
  <Container>
    <TextEditableForm />
  </Container>
);

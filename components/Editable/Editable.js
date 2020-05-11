import React from "react";
import { Button } from "../../components";

const EditableContainer = ({ children }) => (
  <form className="nl3 nr3" onSubmit={e => e.preventDefault()}>
    {children}
  </form>
);

const EditableToolbar = ({ title, buttonLabel }) => (
  <div className="flex flex-wrap items-center bt bb justify-between pv2 mh3">
    <div>{title}</div>
    <div>
      <Button
        variant="primary"
        size="normal"
        type="submit"
        value="value_goes_here"
      >
        {buttonLabel}
      </Button>
    </div>
  </div>
);

const ContentEditableBlock = ({ children }) => (
  <code>
    <pre
      id="c1"
      contentEditable="true"
      spellCheck="false"
      className="editable pv3 ph3 mv0 relative pre overflow-auto"
    >
      {children}
    </pre>
  </code>
);

const TextAreaBlock = ({ children }) => (
  <pre
    id="c1"
    contentEditable="true"
    spellCheck="false"
    className="pv3 ph3 mv0 relative pre overflow-auto"
  >
    <textarea className="editable">{children}</textarea>
  </pre>
);

const ContentEditableForm = () => (
  <EditableContainer>
    <EditableToolbar title="show fetchgit:README.md" buttonLabel="Save" />
    <ContentEditableBlock>testing one two three</ContentEditableBlock>
  </EditableContainer>
);

const TextEditableForm = () => (
  <EditableContainer>
    <EditableToolbar title="show fetchgit:README.md" buttonLabel="Save" />
    <ContentEditableBlock>testing one two three</ContentEditableBlock>
  </EditableContainer>
);

export {
  EditableContainer,
  EditableToolbar,
  ContentEditableBlock,
  ContentEditableForm,
  TextEditableForm
};

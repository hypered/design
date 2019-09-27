import React from "react";
import styled from "styled-components";

function Pre(props) {
  return (
    <pre className="pre overflow-auto">
      <code>{props.children}</code>
    </pre>
  );
}

const EditablePreStyled = styled.pre.attrs(props => ({
  contentEditable: true,
  spellCheck: false,
  className: "pv3 ph3 mv0 relative pre overflow-auto",
}))`
  &[contenteditable="true"]:active,
  &[contenteditable="true"]:focus {
    background: #fff;
    box-shadow: 0 0 0 1px #96ccff;
    outline: none;
  }
`;

function EditablePre(props) {
  return (
    <EditablePreStyled>
      <code>{props.children}</code>
    </EditablePreStyled>
  );
}

function Code(props) {
  return (
    <>
      {props.editable ? (
        <EditablePre>{props.children}</EditablePre>
      ) : (
        <Pre>{props.children}</Pre>
      )}
    </>
  );
}

export { Code };

import styled from "styled-components";

const Pre = props => (
  <pre className="pre overflow-auto">
    <code>{props.children}</code>
  </pre>
);

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

const EditablePre = props => (
  <EditablePreStyled>
    <code>{props.children}</code>
  </EditablePreStyled>
);

export const Code = props => (
  <>
    {props.editable ? (
      <EditablePre>{props.children}</EditablePre>
    ) : (
      <Pre>{props.children}</Pre>
    )}
  </>
);

import { Button } from "../../components";

const EditableForm = ({ children }) => (
  <form onSubmit={e => e.preventDefault()}>{children}</form>
);

const Pre = ({ children }) => (
  <pre className="pre overflow-auto">
    <code className="code">{children}</code>
  </pre>
);

const EditablePre = ({ children }) => (
  <pre
    className="pre hy-editable pv3 ph3 mv0 overflow-auto"
    contentEditable="true"
    spellCheck="false"
  >
    <code className="code">{children}</code>
  </pre>
);

const EditableTextArea = ({ children }) => (
  <pre
    className="pv3 ph3 mv0 relative pre overflow-auto hy-editable"
    spellCheck="false"
  >
    <textarea className="code db w-100 h5 input-reset bn pa0 ma0 outline-0">
      {children}
    </textarea>
  </pre>
);

const CodeToolbar = ({ title }) => (
  <div className="flex flex-wrap items-center bt bb justify-between pv3 mh3">
    <div>{title}</div>
  </div>
);

const CodeToolbarWithButton = ({ title, label }) => (
  <div className="flex flex-wrap items-center bt bb justify-between pv2 mh3">
    <div>{title}</div>
    <div>
      <input
        type="submit"
        value={label}
        className="button-reset bg-black ph3 pv2 white br2 bn"
        onClick={e => e.preventDefault()}
      />
    </div>
  </div>
);

const Code = ({ editable, children }) => (
  <>
    {editable ? <EditablePre>{children}</EditablePre> : <Pre>{children}</Pre>}
  </>
);

export {
  EditableForm,
  EditablePre,
  EditableTextArea,
  CodeToolbar,
  CodeToolbarWithButton,
  Code
};

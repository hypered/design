const Pre = props => (
  <pre className="pre overflow-auto">
    <code>{props.children}</code>
  </pre>
);

const EditablePre = props => (
  <pre
    className="pre hy-editable-pre pv3 ph3 mv0 overflow-auto"
    contentEditable="true"
  >
    <code>{props.children}</code>
  </pre>
);

export const CodeToolbar = props => (
  <div className="flex flex-wrap items-center bt bb justify-between pv2 mh3">
    <div>show fetchgit:README.md</div>
    <div>
      <input
        type="submit"
        value="Save"
        className="button-reset bg-black ph3 pv2 white br2 bn"
      />
    </div>
  </div>
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

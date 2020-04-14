export const ModalOverlay = ({ children, ...props }) => (
  <label className="bg-black-50 absolute absolute--fill z-1" {...props}>
    {children}
  </label>
);

export const ModalContainer = ({ htmlFor, children }) => (
  <label className="modal items-center justify-center h-100 absolute absolute--fill">
    <ModalOverlay htmlFor={htmlFor} />
    <div
      className="bg-white relative z-2 mw6 center"
      style={{ flexShrink: 0, minWidth: "40rem" }}
    >
      {children}
    </div>
  </label>
);

export const ModalHeader = ({ children }) => (
  <div className="flex items-center justify-between ba b--black bw1 pa3 b">
    {children}
  </div>
);

export const ModalBody = ({ children }) => (
  <div className="bl br b--black bw1 pa3">{children}</div>
);

export const ModalFooter = ({ children }) => (
  <div className="flex justify-between">{children}</div>
);

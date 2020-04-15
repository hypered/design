export const ModalOverlay = ({ children, ...props }) => (
  <label className="bg-black-50 absolute absolute--fill z-0" {...props}>
    {children}
  </label>
);

export const ModalContainer = ({ htmlFor, children }) => (
  <label className="modal items-center justify-center h-100 absolute absolute--fill z-1">
    <ModalOverlay htmlFor={htmlFor} />
    <div className="bg-white relative z-3 mw6-m mw6-l center w-100 w-75-m w-50-l mh-75">
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
  <div className="bg-white modal-body bl br b--black bw1 pa3 overflow-y-scroll">
    {children}
  </div>
);

export const ModalFooter = ({ children }) => (
  <div className="bg-blue flex justify-between">{children}</div>
);

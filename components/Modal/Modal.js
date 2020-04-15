export const ModalState = ({ modalName }) => (
  <>
    {/* Using input type="checkbox" to toggle modal  */}
    <input className="modal-state" id={modalName} type="checkbox" />
  </>
);

export const ModalOverlay = ({ children, ...props }) => (
  <label className="bg-black-50 fixed absolute--fill z-1" {...props}>
    {children}
  </label>
);

export const ModalContainer = ({ modalName, htmlFor, children }) => (
  <>
    <ModalState modalName={modalName} />

    <div className="modal items-center justify-center h-100 absolute absolute--fill z-1">
      <div className="bg-white relative z-2 mw6-m mw6-l center w-100 w-75-m w-50-l mh-75">
        {children}
      </div>
      <ModalOverlay htmlFor={htmlFor} />
    </div>
  </>
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

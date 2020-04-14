import React from "react";
import {
  ModalOverlay,
  ModalContainer,
  ModalHeader,
  ModalBody,
  ModalFooter,
  H4,
  Input,
  Button,
  A
} from "../../components";

export default {
  title: "Modal"
};

export const Modal = () => {
  const modalName = "modal";

  return (
    <>
      <Button as="label" htmlFor={modalName} variant="primary" size="normal">
        Open modal
      </Button>

      {/* Using input type="checkbox" to toggle modal  */}
      <input className="modal-state" id="modal" type="checkbox" />

      <ModalContainer htmlFor={modalName}>
        <ModalHeader>
          <div>
            <H4>This is a modal</H4>
          </div>
          <label htmlFor={modalName}>
            <img src="img/close.svg" />
          </label>
        </ModalHeader>
        <ModalBody>
          <Input label="Name" />
          <Input label="Password" />
        </ModalBody>
        <ModalFooter>
          <Button as="label" htmlFor={modalName} variant="primary" size="large">
            Dismiss Modal —>
          </Button>
        </ModalFooter>
      </ModalContainer>
    </>
  );
};

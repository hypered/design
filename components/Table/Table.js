import React from "react";

function Table(props) {
  return (
    <table className="bg-white collapse w-100">
      <thead>{props.th}</thead>
      <tbody>{props.td}</tbody>
    </table>
  );
}

export { Table };

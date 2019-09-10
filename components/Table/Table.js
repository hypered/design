import React from "react";

function Table(props) {
  return (
    <div className="overflow-x-scroll">
      <table className="bg-white collapse w-100" style={{ minWidth: "40rem" }}>
        <thead>{props.th}</thead>
        <tbody>{props.td}</tbody>
      </table>
    </div>
  );
}

export { Table };

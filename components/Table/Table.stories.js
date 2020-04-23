import { Table, TH, TD } from "../../components";

export default {
  title: "Table"
};

const _rows = 10;

export const Default = () => (
  <Table>
    <thead>
      <tr>
        <TH>Column 1</TH>
        <TH>Column 2</TH>
        <TH>Column 3</TH>
        <TH>Column 4</TH>
        <TH align="right">Column 5</TH>
      </tr>
    </thead>
    <tbody>
      {[...Array(_rows)].map((row, i) => (
        <tr key={i}>
          <TD>Red</TD>
          <TD>Green</TD>
          <TD>Blue</TD>
          <TD>Yellow</TD>
          <TD align="right">001</TD>
        </tr>
      ))}
    </tbody>
  </Table>
);

export const Compact = () => (
  <Table>
    <thead>
      <tr>
        <TH size="compact">Column 1</TH>
        <TH size="compact">Column 2</TH>
        <TH size="compact">Column 3</TH>
        <TH size="compact">Column 4</TH>
        <TH size="compact" align="right">
          Column 5
        </TH>
      </tr>
    </thead>
    <tbody>
      {[...Array(_rows)].map((row, i) => (
        <tr key={i}>
          <TD size="compact">Red</TD>
          <TD size="compact">Green</TD>
          <TD size="compact">Blue</TD>
          <TD size="compact">Yellow</TD>
          <TD size="compact" align="right">
            001
          </TD>
        </tr>
      ))}
    </tbody>
  </Table>
);

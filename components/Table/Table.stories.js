import { Table, TH1, TH2, TD1, TD2 } from "../../components";

export default {
  title: "Table",
};

const _rows = 10;

export const Default = () => (
  <Table>
    <thead>
      <tr>
        <TH1>Column 1</TH1>
        <TH1>Column 2</TH1>
        <TH1>Column 3</TH1>
        <TH1>Column 4</TH1>
        <TH1>
          <div className="tr">Column 5</div>
        </TH1>
      </tr>
    </thead>
    <tbody>
      {[...Array(_rows)].map(row => (
        <tr>
          <TD1>Red</TD1>
          <TD1>Green</TD1>
          <TD1>Blue</TD1>
          <TD1>Yellow</TD1>
          <TD1>
            <div className="tr">001</div>
          </TD1>
        </tr>
      ))}
    </tbody>
  </Table>
);

export const Compact = () => (
  <Table>
    <thead>
      <tr>
        <TH2>Column 1</TH2>
        <TH2>Column 2</TH2>
        <TH2>Column 3</TH2>
        <TH2>Column 4</TH2>
        <TH2>
          <div className="tr">Column 5</div>
        </TH2>
      </tr>
    </thead>
    <tbody>
      {[...Array(_rows)].map(row => (
        <tr>
          <TD2>Red</TD2>
          <TD2>Green</TD2>
          <TD2>Blue</TD2>
          <TD2>Yellow</TD2>
          <TD2>
            <div className="tr">001</div>
          </TD2>
        </tr>
      ))}
    </tbody>
  </Table>
);

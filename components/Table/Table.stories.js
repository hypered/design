import { Table, TR, TH, TD } from "../../components";

export default {
  title: "Table"
};

const _rows = 10;

const TableTemplate = ({ isHeader, size, colDivider, align }) => (
  <Table>
    <thead>
      <TR isHeader>
        <TH size={size} colDivider={colDivider}>
          Column 1
        </TH>
        <TH size={size} colDivider={colDivider}>
          Column 2
        </TH>
        <TH size={size} colDivider={colDivider}>
          Column 3
        </TH>
        <TH size={size} colDivider={colDivider}>
          Column 4
        </TH>
        <TH size={size} align={"right"}>
          Column 5
        </TH>
      </TR>
    </thead>

    <tbody>
      {[...Array(_rows)].map((row, i, arr) => {
        let hideBottomBorder;
        if (colDivider) {
          if (arr.length === i + 1) {
            hideBottomBorder = true;
          } else {
            hideBottomBorder = false;
          }
        }
        return (
          <TR key={i} hideBottomBorder={hideBottomBorder}>
            <TD size={size} colDivider={colDivider}>
              Red
            </TD>
            <TD size={size} colDivider={colDivider}>
              Green
            </TD>
            <TD size={size} colDivider={colDivider}>
              Blue
            </TD>
            <TD size={size} colDivider={colDivider}>
              Yellow
            </TD>
            <TD size={size} align="right">
              001
            </TD>
          </TR>
        );
      })}
    </tbody>
  </Table>
);

export const Default = () => <TableTemplate size="normal" />;

export const Compact = () => <TableTemplate size="compact" />;

export const WithColumnDivider = () => (
  <TableTemplate size="normal" colDivider />
);

export const WithColumnDividerCompact = () => (
  <TableTemplate size="compact" colDivider />
);

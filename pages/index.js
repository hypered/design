import React from "react";
import styled from "styled-components";
import { space, width, fontSize, color, flex } from "styled-system";

const Box = styled.div`
  ${space}
  ${width}
  ${fontSize}
  ${color}
  ${flex}
  background: blue;
  color: white;
`;

function Home() {
  return <Box>Hello world!</Box>;
}

export default Home;

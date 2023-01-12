import {
  Layout,
  H1,
  P,
  SidePanel,
  SidePanelTitle,
  SidePanelUL,
  SidePanelLI,
  SidePanelLink,
} from "../../components";

export default {
  title: "Side Panel",
};

export const Default = () => (
  <SidePanel>
    <SidePanelTitle>Latest Runs</SidePanelTitle>
    <SidePanelUL>
      <SidePanelLI>
        <SidePanelLink href='#'>#001</SidePanelLink>
      </SidePanelLI>
      <SidePanelLI>
        <SidePanelLink href='#'>#002</SidePanelLink>
      </SidePanelLI>
      <SidePanelLI>
        <SidePanelLink href='#'>#003</SidePanelLink>
      </SidePanelLI>
      <SidePanelLI>
        <SidePanelLink href='#'>#004</SidePanelLink>
      </SidePanelLI>
      <SidePanelLI>
        <SidePanelLink href='#'>#005</SidePanelLink>
      </SidePanelLI>
    </SidePanelUL>
  </SidePanel>
);

export const Usage = () => (
  <Layout>
    <div className="flex flex-wrap nl3 nr3">
      <main className="w-100 w-80-m w-80-l ph3">
        <article>
          <H1>Waveguide</H1>
          <P>
            If neither a list of attribute names or a command are given,
            Waveguide instrospects the Nix expression and builds all the found
            attributes.
          </P>
        </article>
      </main>
      <Default />
    </div>
  </Layout>
);

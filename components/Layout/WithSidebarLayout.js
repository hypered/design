import {
  Layout,
  Sidebar,
  SidePanel,
  SidePanelTitle,
  SidePanelUL,
  SidePanelLI,
  SidePanelLink,
  UL,
  OL,
  LI,
  H1,
  H2,
  H3,
  H4,
  P,
  BlockQuote,
  PullQuote,
  Divider,
} from "../../components";

export const WithSidebarLayout = props => (
  <Layout>
    <main className="flex flex-wrap nl3 nr3">
      <Sidebar />

      <article className="order-0 order-1-m order-1-l w-100 m-60-m w-60-l ph3 cf">
        <div className="mb4">
          <H1>Starting with NixOps (and thus Nix and NixOS)</H1>
          <Divider />
        </div>

        <H2>Introduction</H2>

        <P>
          Given the three above derivations, it is possible to generate the
          appropriate qemu-kvm invocation as a script, runvm. runvm is the main
          entry point to start playing and understanding not-os. Follow the
          link, and enjoy!
        </P>

        <UL>
          <LI>a kernel (config.system.build.kernel)</LI>
          <LI>an initrd (config.system.build.initialRamdisk)</LI>
          <LI>a rootfs (config.system.build.squashfs)</LI>
        </UL>

        <P>
          This is a project of Michael Bishop (cleverca22 on GitHub, clever on
          IRC). I modified it just a bit to make it possible to generate this
          documentation.
        </P>

        <H3>Sub-points</H3>

        <OL>
          <LI>Item one</LI>
          <LI>Item two</LI>
          <LI>Item three</LI>
        </OL>

        <H4>Some quotes</H4>
        <BlockQuote>
          To follow along, you can clone the Git repository and run each
          nix-build command as they appear at the top of each page.
        </BlockQuote>

        <H3>Ending points</H3>

        <PullQuote>
          To follow along, you can clone the Git repository and run each
          nix-build command as they appear at the top of each page.
        </PullQuote>

        <P>
          Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
          eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad
          minim veniam, quis nostrud exercitation ullamco laboris nisi ut
          aliquip ex ea commodo consequat. Duis aute irure dolor in
          reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla
          pariatur. Excepteur sint occaecat cupidatat non proident, sunt in
          culpa qui officia deserunt mollit anim id est laborum.
        </P>
      </article>

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
    </main>
  </Layout>
);

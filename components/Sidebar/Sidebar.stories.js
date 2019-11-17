import { Layout, Sidebar, H1, H2, P } from "../../components";

export default {
  title: "Sidebar",
};

export const Default = () => <Sidebar />;

export const UsageExample = () => (
  <Layout>
    <main className="flex flex-wrap nl3 nr3">
      <Sidebar />

      <section className="order-0 order-1-m order-1-l w-100 w-75-m w-75-l ph3">
        <article>
          <H1>not-os</H1>
          <P>
            not-os is a minimal OS based on the Linux kernel, coreutils, runit,
            and Nix. It is also the build script, written in Nix expressions, to
            build such OS.
          </P>
          <P>
            This is a project of Michael Bishop (cleverca22 on GitHub, clever on
            IRC). I modified it just a bit to make it possible to generate this
            documentation.
          </P>
          <P>
            As a build tool, not-os uses nixpkgs and in particular the{" "}
            <a href="https://nixos.wiki/wiki/NixOS_Modules">
              NixOS module system
            </a>{" "}
            to build the three main components of a Linux-based operating
            system:
          </P>
        </article>
      </section>
    </main>
  </Layout>
);

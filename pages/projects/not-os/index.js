import React from "react";
import Link from "next/link";

import { Layout } from "../../../components";

function NavTitle(props) {
  return <h3 className="f5 ttu mv1">{props.children}</h3>;
}

function NavUL(props) {
  return <ul className="list pl0 mb3 mt0">{props.children}</ul>;
}

function NavLI(props) {
  return <li>{props.children}</li>;
}

function NavLink(props) {
  return (
    <Link href={props.href}>
      <a className="link black hover-blue">{props.children}</a>
    </Link>
  );
}

function Nav(props) {
  return (
    <nav className="w-20">
      <NavTitle>Intro</NavTitle>
      <NavUL>
        <NavLI>
          <NavLink href="../../projects/not-os/index.html">not-os</NavLink>
        </NavLI>
      </NavUL>

      <NavTitle>Notes</NavTitle>
      <NavUL>
        <NavLI>
          <NavLink href="../../projects/not-os/digital-ocean.html">
            Digital Ocean
          </NavLink>
        </NavLI>

        <NavLI>
          <NavLink href="../../projects/not-os/todo.html">TODO</NavLink>
        </NavLI>
      </NavUL>

      <NavTitle>Main Attributes</NavTitle>
      <NavUL>
        <NavLI>
          <NavLink href="../../projects/not-os/runvm.html">runvm</NavLink>
        </NavLI>
        <NavLI>
          <NavLink href="../../projects/not-os/kernel.html">kernel</NavLink>
        </NavLI>
        <NavLI>
          <NavLink href="../../projects/not-os/initrd.html">initrd</NavLink>
        </NavLI>
        <NavLI>
          <NavLink href="../../projects/not-os/rootfs.html">rootfs</NavLink>
        </NavLI>
        <NavLI>
          <NavLink href="../../projects/not-os/ext4.html">ext4</NavLink>
        </NavLI>
        <NavLI>
          <NavLink href="../../projects/not-os/stage-1.html">stage-1</NavLink>
        </NavLI>
        <NavLI>
          <NavLink href="../../projects/not-os/stage-2.html">stage-2</NavLink>
        </NavLI>
        <NavLI>
          <NavLink href="../../projects/not-os/dist.html">dist</NavLink>
        </NavLI>
        <NavLI>
          <NavLink href="../../projects/not-os/extra-utils.html">
            extra-utils
          </NavLink>
        </NavLI>
        <NavLI>
          <NavLink href="../../projects/not-os/path.html">path</NavLink>
        </NavLI>
        <NavLI>
          <NavLink href="../../projects/not-os/shrunk.html">shrunk</NavLink>
        </NavLI>
        <NavLI>
          <NavLink href="../../projects/not-os/toplevel.html">toplevel</NavLink>
        </NavLI>
        <NavLI>
          <NavLink href="../../projects/not-os/boot.html">boot</NavLink>
        </NavLI>
        <NavLI>
          <NavLink href="../../projects/not-os/ext4.html">ext4</NavLink>
        </NavLI>
        <NavLI>
          <NavLink href="../../projects/not-os/raw.html">raw</NavLink>
        </NavLI>
        <NavLI>
          <NavLink href="../../projects/not-os/qcow2.html">qcow2</NavLink>
        </NavLI>
        <NavLI>
          <NavLink href="../../projects/not-os/syslinux.html">syslinux</NavLink>
        </NavLI>
      </NavUL>

      <NavTitle>Values</NavTitle>
      <NavUL>
        <NavLI>
          <NavLink href="../../projects/not-os/cmdline.html">cmdline</NavLink>
        </NavLI>
        <NavLI>
          <NavLink href="../../projects/not-os/root-modules.html">
            root-modules
          </NavLink>
        </NavLI>
      </NavUL>

      <NavTitle>Source</NavTitle>
      <NavUL>
        <NavLI>
          <NavLink href="../../projects/not-os/default.html">
            default.nix
          </NavLink>
        </NavLI>
      </NavUL>
    </nav>
  );
}

function Article(props) {
  return (
    <section className="w-100 w-80-m w-80-l ph3">
      <article className="measure-wide">
        <h1 className="f1 lh-title mv0">not-os</h1>
        <p>
          not-os is a minimal OS based on the Linux kernel, coreutils, runit,
          and Nix. It is also the build script, written in Nix expressions, to
          build such OS.
        </p>
        <p>
          This is a project of Michael Bishop (cleverca22 on GitHub, clever on
          IRC). I modified it just a bit to make it possible to generate this
          documentation.
        </p>
        <p>
          As a build tool, not-os uses nixpkgs and in particular the
          <a href="https://nixos.wiki/wiki/NixOS_Modules">
            NixOS module system
          </a>
          to build the three main components of a Linux-based operating system:
        </p>
        <ul>
          <li>
            a kernel (<code>config.system.build.kernel</code>)
          </li>
          <li>
            an initrd (<code>config.system.build.initialRamdisk</code>)
          </li>
          <li>
            a rootfs (<code>config.system.build.squashfs</code>)
          </li>
        </ul>
        <p>
          Given the three above derivations, it is possible to generate the
          appropriate qemu-kvm invocation as a script,
          <a href="runvm.html">runvm</a>. runvm is the main entry point to start
          playing and understanding not-os. Follow the link, and enjoy!
        </p>
        <p>
          <br />
          <br />
          <em>
            To follow along, you can clone the Git repository and run each
            <code>nix-build</code> command as they appear at the top of each
            page.
          </em>
        </p>
      </article>
    </section>
  );
}

function NavBlock(props) {
  return (
    <nav className="bg-near-white pa4 db w-100 mw8 mt5 br1">
      {props.children}
    </nav>
  );
}

function NavBlockRow(props) {
  return <div className="flex flex-wrap nl3 nr3">{props.children}</div>;
}

function NavBlockCol(props) {
  return <div className="w-100 w-50-m w-25-l ph3">{props.children}</div>;
}

function NotOS(props) {
  return (
    <Layout>
      <div className="flex">
        <Nav />
        <Article />
      </div>

      <section>
        <NavBlock>
          <NavBlockRow>
            <NavBlockCol>
              <NavTitle>Main Attributes</NavTitle>
              <NavUL>
                <NavLI>
                  <NavLink href="../../projects/not-os/runvm.html">
                    runvm
                  </NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/kernel.html">
                    kernel
                  </NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/initrd.html">
                    initrd
                  </NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/rootfs.html">
                    rootfs
                  </NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/ext4.html">ext4</NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/stage-1.html">
                    stage-1
                  </NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/stage-2.html">
                    stage-2
                  </NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/dist.html">dist</NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/extra-utils.html">
                    extra-utils
                  </NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/path.html">path</NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/shrunk.html">
                    shrunk
                  </NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/toplevel.html">
                    toplevel
                  </NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/boot.html">boot</NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/ext4.html">ext4</NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/raw.html">raw</NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/qcow2.html">
                    qcow2
                  </NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/syslinux.html">
                    syslinux
                  </NavLink>
                </NavLI>
              </NavUL>
            </NavBlockCol>

            <NavBlockCol>
              <NavTitle>Main Attributes</NavTitle>
              <NavUL>
                <NavLI>
                  <NavLink href="../../projects/not-os/runvm.html">
                    runvm
                  </NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/kernel.html">
                    kernel
                  </NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/initrd.html">
                    initrd
                  </NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/rootfs.html">
                    rootfs
                  </NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/ext4.html">ext4</NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/stage-1.html">
                    stage-1
                  </NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/stage-2.html">
                    stage-2
                  </NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/dist.html">dist</NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/extra-utils.html">
                    extra-utils
                  </NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/path.html">path</NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/shrunk.html">
                    shrunk
                  </NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/toplevel.html">
                    toplevel
                  </NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/boot.html">boot</NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/ext4.html">ext4</NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/raw.html">raw</NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/qcow2.html">
                    qcow2
                  </NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/syslinux.html">
                    syslinux
                  </NavLink>
                </NavLI>
              </NavUL>
            </NavBlockCol>

            <NavBlockCol>
              <NavTitle>Values</NavTitle>
              <NavUL>
                <NavLI>
                  <NavLink href="../../projects/not-os/cmdline.html">
                    cmdline
                  </NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/root-modules.html">
                    root-modules
                  </NavLink>
                </NavLI>
              </NavUL>

              <NavTitle>Source</NavTitle>
              <NavUL>
                <NavLI>
                  <NavLink href="../../projects/not-os/default.html">
                    default.nix
                  </NavLink>
                </NavLI>
              </NavUL>
            </NavBlockCol>

            <NavBlockCol>
              <NavTitle>Values</NavTitle>
              <NavUL>
                <NavLI>
                  <NavLink href="../../projects/not-os/cmdline.html">
                    cmdline
                  </NavLink>
                </NavLI>
                <NavLI>
                  <NavLink href="../../projects/not-os/root-modules.html">
                    root-modules
                  </NavLink>
                </NavLI>
              </NavUL>

              <NavTitle>Source</NavTitle>
              <NavUL>
                <NavLI>
                  <NavLink href="../../projects/not-os/default.html">
                    default.nix
                  </NavLink>
                </NavLI>
              </NavUL>
            </NavBlockCol>
          </NavBlockRow>
        </NavBlock>
      </section>
    </Layout>
  );
}

export default NotOS;

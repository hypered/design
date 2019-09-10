import React from "react";

import { Layout, H1, H2, P } from "../../components";

function NubsBash(props) {
  return (
    <Layout>
      <article>
        <H1>Starting with NixOps (and thus Nix and NixOS), part 1</H1>
        <P>
          While learning the Nix ecosystem and trying to use it, I found it a
          bit more harder than I thought to achieve what I wanted. In this post
          and the next one, I’m documenting what I learned, partly for myself,
          partly to share with other people that would like to follow the same
          path.
        </P>
        <P>
          The prerequesite to follow along this post is to install Nix and
          NixOps.
        </P>
        <H2 id="basics">Basics</H2>
        <P>
          The basics of Nix are actually very well documented elsewhere; in
          particular:
        </P>
        <ul>
          <li>
            <a href="http://nixos.org/nix/manual/">The Nix manual</a>
          </li>
          <li>
            <a href="https://medium.com/@MrJamesFisher/nix-by-example-a0063a1a4c55">
              Nix by example
            </a>
          </li>
          <li>
            <a href="http://lethalman.blogspot.be/2014/07/nix-pill-1-why-you-should-give-it-try.html">
              The Nix pills series
            </a>
          </li>
        </ul>
        <H2 id="running-example">Running example</H2>
        <P>
          As as a starting point, here I give a <code>do.nix</code> file
          suitable for the <code>nixops</code> executable. I will use it in the
          rest of the post as a running example:
        </P>
        <pre>
          <code>{`
$ cat do.nix
{
  network.description = &quot;Some machines (actually just one)&quot;;

  resources.sshKeyPairs.ssh-key = {};

  machine-1 = { config, pkgs, ... }: {
    deployment.targetEnv = &quot;digitalOcean&quot;;
    deployment.digitalOcean.region = &quot;ams2&quot;;
    deployment.digitalOcean.size = &quot;512mb&quot;;

  }; # machine-1
}
  `}</code>
        </pre>
        <P>
          That file contains a single Nix expression and is enough to get a
          machine up and running on Digital Ocean. (It seems the support for the
          AWS environment is much more mature but for what this post is about,
          Digital Ocean support is fine.)
        </P>
        <P>
          Here is how you instruct NixOps to use that file to spin up a machine
          and provision it (you can create API tokens at
          https://cloud.digitalocean.com/settings/api/tokens):
        </P>
      </article>
    </Layout>
  );
}

export default NubsBash;

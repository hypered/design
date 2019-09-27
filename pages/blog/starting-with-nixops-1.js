import React from "react";

import { Layout, H1, H2, P, Code } from "../../components";

function Nixops() {
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

        <P>
          As as a starting point, here I give a <code>do.nix</code> file
          suitable for the <code>nixops</code> executable. I will use it in the
          rest of the post as a running example:
        </P>

        <Code>{`$ cat do.nix
{
  network.description = &quot;Some machines (actually just one)&quot;;

  resources.sshKeyPairs.ssh-key = {};

  machine-1 = { config, pkgs, ... }: {
    deployment.targetEnv = &quot;digitalOcean&quot;;
    deployment.digitalOcean.region = &quot;ams2&quot;;
    deployment.digitalOcean.size = &quot;512mb&quot;;

  }; # machine-1
}`}</Code>

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

        <Code>{`$ nixops create -d do do.nix
$ nixops list
+----------------+------+------------------------+------------+--------------+
| UUID           | Name | Description            | # Machines |     Type     |
+----------------+------+------------------------+------------+--------------+
| c94aa2ee-7954- | do   | Unnamed NixOps network |          0 |              |
+----------------+------+------------------------+------------+--------------+
$ nixops info -d do
Network name: do
Network UUID: c94aa2ee-7954-11e7-8947-0242c5c1eaab
Network description: Some machines (actually just one)
Nix expressions: /home/thu/projects/web/nixops/do.nix

+-----------+---------------+---------------------+-------------+------------+
| Name      |     Status    | Type                | Resource Id | IP address |
+-----------+---------------+---------------------+-------------+------------+
| machine-1 | Missing / New | digitalOcean [ams2] |             |            |
+-----------+---------------+---------------------+-------------+------------+
$ DIGITAL_OCEAN_AUTH_TOKEN=xxxx nixops deploy -d do
machine-1&gt; creating droplet ...
...`}</Code>

        <P>
          This creates a “deployment” using our expression: a mean for NixOps to
          track the state associated with our machines and name that deployment
          “do” (which can be used instead of its UUID) then actually deploy it
          to Digital Ocean. (For good measure I call also{" "}
          <code>nixops list</code> and <code>nixops info</code> to demonstrate
          those two commands.)
        </P>

        <P>
          After a while (a long while when using the Digital Ocean target), you
          should be able to SSH into the machine:
        </P>

        <Code>{`
machine-1&gt; activation finished successfully
do&gt; deployment finished successfully
$ nixops ssh -d do machine-1
[root@machine-1:~]#</code></pre>
<P>For good measure again:</P>
<pre><code>$ nixops list
+----------------+------+-----------------------------------+------------+--------------+
| UUID           | Name | Description                       | # Machines |     Type     |
+----------------+------+-----------------------------------+------------+--------------+
| c94aa2ee-7954- | do   | Some machines (actually just one) |          1 | digitalOcean |
+----------------+------+-----------------------------------+------------+--------------+
$ nixops info -d do
Network name: do
Network UUID: c94aa2ee-7954-11e7-8947-0242c5c1eaab
Network description: Some machines (actually just one)
Nix expressions: /home/thu/projects/web/nixops/do.nix

+-----------+-----------------+--------------+-------------+----------------+
| Name      |      Status     | Type         | Resource Id | IP address     |
+-----------+-----------------+--------------+-------------+----------------+
| machine-1 | Up / Up-to-date | digitalOcean |             | 188.226.174.95 |
| ssh-key   | Up / Up-to-date | ssh-keypair  |             |                |
+-----------+-----------------+--------------+-------------+----------------+`}</Code>

        <H2 id="deploying-changes">Deploying changes</H2>
        <P>
          When you want to update the machine, following changes to the Nix
          expression describing it, the same <code>deploy</code> command is used
          again. (You can already do it again with no changes and it should
          complete much more quickly.)
        </P>
        <H2 id="destroying-the-machine">Destroying the machine</H2>
        <P>You can destroy the droplet with the following command:</P>
        <Code>{`$ DIGITAL_OCEAN_AUTH_TOKEN=xxxx nixops destroy -d do
machine-1&gt; destroying droplet 57418645`}</Code>
        <H2 id="next">Next</H2>
        <P>
          <a href="starting-with-nixops-2.html">Part 2 is here.</a>
        </P>
      </article>
    </Layout>
  );
}

export default Nixops;

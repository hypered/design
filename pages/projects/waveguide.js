import React from "react";
import Link from "next/link";

import { Layout } from "../../components";

function WaveguideArticle(props) {
  return (
    <article className="">
      <h1 className="f2 lh-title mv2">Waveguide</h1>
      <p>
        Waveguide builds and/or runs a Nix expression on a freshly provisioned
        machine.
      </p>
      <p>Waveguide performs the following six steps:</p>
      <ol>
        <li>
          <code>doctl compute droplet create</code>
        </li>
        <li>
          <code>nix-x.x-install.sh</code>
        </li>
        <li>
          <code>git clone</code>
        </li>
        <li>
          <code>nix-build</code> or <code>nix run</code>
        </li>
        <li>
          <code>s3cmd sync</code>
        </li>
        <li>
          <code>doctl compute droplet delete</code>
        </li>
      </ol>
      <hr />
      <h2 id="credentials">Credentials</h2>
      <p>The steps 1, 2, and 3 require credentials:</p>
      <ul>
        <li>Creating a new Dropet on DigitalOcean requires an API key.</li>
        <li>
          Cloning a Git repository through the Git protocol requires a private
          SSH deploy key.
        </li>
        <li>
          Uploading the build logs, and optionaly the Nix results, requires a
          secret S3 access key.
        </li>
      </ul>
      <h2 id="input">Input</h2>
      <p>
        Beside the credentials described above, Waveguide is parametrized with:
      </p>
      <ul>
        <li>The Nix install script version.</li>
        <li>The Git repository URL and revision.</li>
        <li>Optionaly the name Nix expression attribute names to build.</li>
        <li>Optionaly the command to run with Nix.</li>
        <li>The S3 path to upload the logs.</li>
        <li>Optionaly the S3 path to cache the Nix results.</li>
      </ul>
      <p>
        If neither a list of attribute names or a command are given, Waveguide
        instrospects the Nix expression and builds all the found attributes.
      </p>
      <h2 id="output">Output</h2>
      <p>Waveguide produces logs and optionaly populate a Nix cache on S3.</p>
    </article>
  );
}

function AsideTitle(props) {
  return <h3 className="f5 lh-title mv2">{props.children}</h3>;
}

function AsideLI(props) {
  return <li className="pv1 bb b--black-10">{props.children}</li>;
}

function AsideLink(props) {
  return (
    <Link href={props.href}>
      <a className="link no-underline black blue-hover">
        &rarr; {props.children}
      </a>
    </Link>
  );
}

function WaveguideAside(props) {
  return (
    <aside className="w-100 w-20-m w-20-l ph3 mt0 mt5-m mt5-l">
      <div className="nl3 nr3">
        <AsideTitle>Latest Runs</AsideTitle>
        <ul className="bg-near-white list pa3">
          <AsideLI>
            <AsideLink
              href="../run/282/provisioning.html"
              className="black hover-blue b"
            >
              #282
            </AsideLink>
          </AsideLI>
          <AsideLI>
            <AsideLink href="../run/281/provisioning.html">#281</AsideLink>
          </AsideLI>
          <AsideLI>
            <AsideLink href="../run/280/provisioning.html">#280</AsideLink>
          </AsideLI>
          <AsideLI>
            <AsideLink href="../run/279/provisioning.html">#279</AsideLink>
          </AsideLI>
          <AsideLI>
            <AsideLink href="../run/278/provisioning.html">#278</AsideLink>
          </AsideLI>
          <AsideLI>
            <AsideLink href="../run/277/provisioning.html">#277</AsideLink>
          </AsideLI>
          <AsideLI>
            <AsideLink href="../run/276/provisioning.html">#276</AsideLink>
          </AsideLI>
          <AsideLI>
            <AsideLink href="../run/275/provisioning.html">#275</AsideLink>
          </AsideLI>
          <AsideLI>
            <AsideLink href="../run/274/provisioning.html">#274</AsideLink>
          </AsideLI>
          <AsideLI>
            <AsideLink href="../run/273/provisioning.html">#273</AsideLink>
          </AsideLI>
          <AsideLI>
            <AsideLink href="../run/272/provisioning.html">#272</AsideLink>
          </AsideLI>
          <AsideLI>
            <AsideLink href="../run/268/provisioning.html">#268</AsideLink>
          </AsideLI>
          <AsideLI>
            <AsideLink href="../run/267/provisioning.html">#267</AsideLink>
          </AsideLI>
          <AsideLI>
            <AsideLink href="../run/266/provisioning.html">#266</AsideLink>
          </AsideLI>
          <AsideLI>
            <AsideLink href="../run/265/provisioning.html">#265</AsideLink>
          </AsideLI>
          <AsideLI>
            <AsideLink href="../run/264/provisioning.html">#264</AsideLink>
          </AsideLI>
          <AsideLI>
            <AsideLink href="../run/263/provisioning.html">#263</AsideLink>
          </AsideLI>
        </ul>
      </div>
    </aside>
  );
}

function Waveguide(props) {
  return (
    <Layout>
      <div className="flex flex-wrap nl3 nr3">
        <section className="w-100 w-80-m w-80-l ph3">
          <WaveguideArticle />
        </section>

        <WaveguideAside />
      </div>
    </Layout>
  );
}

export default Waveguide;

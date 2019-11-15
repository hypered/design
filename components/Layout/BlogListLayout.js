import { Layout, H1, H2, H3, H4, H5, P } from "../../components";

export const BlogListLayout = props => (
  <Layout>
    <main>
      <H1>The Hypered Publication</H1>

      <ul className="list pl0 mw6 mt4">
        <li className="pv3">
          <H3>Starting with NixOps (and thus Nix and NixOS)</H3>
          <time datetime="2018-12-08">2019-08-21</time>
          <P>
            Duis aute irure dolor in reprehenderit in voluptate velit esse
            cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat
            cupidatat non proident, sunt in culpa qui officia deserunt mollit
            anim id est laborum.
          </P>
          <a href="#" className="black b no-underline">
            Continue reading...
          </a>
        </li>

        <li className="pv3">
          <H3>
            Exposing a local server through HAProxy using a reverse SSH tunnel
          </H3>
          <time datetime="2018-12-08">2019-08-21</time>
          <P>
            Duis aute irure dolor in reprehenderit in voluptate velit esse
            cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat
            cupidatat non proident, sunt in culpa qui officia deserunt mollit
            anim id est laborum.
          </P>
          <a href="#" className="black b no-underline">
            Continue reading...
          </a>
        </li>
      </ul>
    </main>
  </Layout>
);

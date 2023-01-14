import {
  Layout,
  A,
  UL,
  OL,
  LI,
  H1,
  H2,
  H3,
  H4,
  HR,
  P,
  Code,
  BlockQuote,
  PullQuote,
  Divider,
  NavBlockWrapper,
  SidebarTitle,
  SidebarUL,
  SidebarLI,
  SidebarLink,
} from "../../components";

export const BlogPost1Layout = props => (
  <Layout copyright="Võ Minh Thu, 2017-2023.">
    <main>
      <article className="mw7 cf">
        <div className="mb4">
          <H1>Static binaries</H1>
        </div>

        <P>
	  If you consider adopting a programming language, consider one where
          you can create statically-linked executables. This makes things so much
          easier.
        </P>

        <HR/>

        <H2>E.g., compared to Docker</H2>

        <P>
	  With a statically-linked binary, provisioning a program on a remote
          host is just uploading it. With Docker, you need to install and
          maintain docker-engine. Ideally you should have a nice way to provision
          the remote host with Docker, but then, why not use that nice way to
          provision other stuff directly ?
        </P>

        <P>
	  Docker is much more than packaging, but it seems a lot of people use
          it for that purpose.
        </P>

        <P>
	  By the way, statically-linked executables make building Docker images
          a breeze.
        </P>

        <P>
	  If you use Docker to deploy, you probably want a private registry,
          which is another piece to maintain. Static binaries are trivial to host
          on a static site. Or to mirror. And so on. Simplicity goes a long way.
        </P>

        <HR/>

        <P>
	  See also: <A href="#">In praise of simplicity</A>
        </P>

        <HR/>

        <P>
	  Related: <A href="#">Learn packaging</A>
        </P>

      </article>
    </main>
  </Layout>
);

import {
  Layout,
  Sidebar,
  H1,
  H2,
  P,
  NavBlockWrapper,
  SidebarTitle,
  SidebarUL,
  SidebarLI,
  SidebarLink,
} from "../../components";

export default {
  title: "Navigation Block",
};

export const Default = () => (
  <NavBlockWrapper>
    <div className="flex flex-wrap nl3 nr3">
      <div className="w-100 w-25-m w-25-l ph3">
        <SidebarTitle>Column 1</SidebarTitle>
        <SidebarUL>
          <SidebarLI>
            <SidebarLink href="#">Item One</SidebarLink>
          </SidebarLI>
          <SidebarLI>
            <SidebarLink href="#">Item Two</SidebarLink>
          </SidebarLI>
          <SidebarLI>
            <SidebarLink href="#">Item Three</SidebarLink>
          </SidebarLI>
          <SidebarLI>
            <SidebarLink href="#">Item Four</SidebarLink>
          </SidebarLI>
          <SidebarLI>
            <SidebarLink href="#">Item Five</SidebarLink>
          </SidebarLI>
        </SidebarUL>
      </div>

      <div className="w-100 w-25-m w-25-l ph3">
        <SidebarTitle>Column 2</SidebarTitle>
        <SidebarUL>
          <SidebarLI>
            <SidebarLink href="#">Item One</SidebarLink>
          </SidebarLI>
          <SidebarLI>
            <SidebarLink href="#">Item Two</SidebarLink>
          </SidebarLI>
          <SidebarLI>
            <SidebarLink href="#">Item Three</SidebarLink>
          </SidebarLI>
          <SidebarLI>
            <SidebarLink href="#">Item Four</SidebarLink>
          </SidebarLI>
          <SidebarLI>
            <SidebarLink href="#">Item Five</SidebarLink>
          </SidebarLI>
        </SidebarUL>
      </div>

      <div className="w-100 w-25-m w-25-l ph3">
        <SidebarTitle>Column 3</SidebarTitle>

        <SidebarUL>
          <SidebarLI>
            <SidebarLink href="#">Item One</SidebarLink>
          </SidebarLI>
          <SidebarLI>
            <SidebarLink href="#">Item Two</SidebarLink>
          </SidebarLI>
          <SidebarLI>
            <SidebarLink href="#">Item Three</SidebarLink>
          </SidebarLI>
          <SidebarLI>
            <SidebarLink href="#">Item Four</SidebarLink>
          </SidebarLI>
          <SidebarLI>
            <SidebarLink href="#">Item Five</SidebarLink>
          </SidebarLI>
        </SidebarUL>
      </div>

      <div className="w-100 w-25-m w-25-l ph3">
        <SidebarTitle>Column 4 (Section 1)</SidebarTitle>
        <SidebarUL>
          <SidebarLI>
            <SidebarLink href="#">Item One</SidebarLink>
          </SidebarLI>
          <SidebarLI>
            <SidebarLink href="#">Item Two</SidebarLink>
          </SidebarLI>
          <SidebarLI>
            <SidebarLink href="#">Item Three</SidebarLink>
          </SidebarLI>
          <SidebarLI>
            <SidebarLink href="#">Item Four</SidebarLink>
          </SidebarLI>
          <SidebarLI>
            <SidebarLink href="#">Item Five</SidebarLink>
          </SidebarLI>
        </SidebarUL>

        <SidebarTitle>Column 4 (Section 2)</SidebarTitle>
        <SidebarUL>
          <SidebarLI>
            <SidebarLink href="#">Item One</SidebarLink>
          </SidebarLI>
          <SidebarLI>
            <SidebarLink href="#">Item Two</SidebarLink>
          </SidebarLI>
          <SidebarLI>
            <SidebarLink href="#">Item Three</SidebarLink>
          </SidebarLI>
        </SidebarUL>
      </div>
    </div>
  </NavBlockWrapper>
);

export const Usage = () => (
  <Layout>
    <div className="flex flex-wrap nl3 nr3">
      <Sidebar />
      <main className="w-100 w-75-m w-75-l ph3">
        <article>
          <H1>Heading 1</H1>
          <P>
            Lorem ipsum dolor sit amet, consectetur adipiscing elit.
            Pellentesque eu magna pharetra, ultricies nisi eget, aliquet nulla.
            Curabitur in ultricies diam. In auctor neque ante, at vulputate
            magna volutpat nec. Morbi pharetra metus vitae dignissim tincidunt.
            Nunc iaculis consectetur purus, quis egestas orci gravida pulvinar.
            Quisque sed malesuada arcu. Sed nisl mi, lobortis eget diam in,
            facilisis tincidunt mi. Sed et neque laoreet, placerat risus ac,
            volutpat ex. Suspendisse venenatis lectus id turpis congue, a
            interdum est viverra. Etiam et nunc et mauris mattis pulvinar.
            Aliquam mattis lacinia molestie. Maecenas molestie ornare
            sollicitudin.
          </P>

          <P>
            Mauris pretium velit eget turpis rhoncus volutpat. Suspendisse sit
            amet egestas lorem. Cras posuere ac tortor vel interdum. Mauris
            ultrices euismod dui ac placerat. Aliquam aliquet erat id mauris
            placerat fermentum. Pellentesque ut sodales ipsum. Donec gravida,
            dui at tempus pretium, dolor urna aliquet est, mattis pellentesque
            odio elit a massa. Morbi ut efficitur tortor. Pellentesque ut dolor
            et augue mollis accumsan et sit amet urna. Mauris ac eros non mauris
            sodales posuere. Nunc a tortor eget quam laoreet suscipit. Phasellus
            sollicitudin suscipit libero.
          </P>

          <H2>Heading 2</H2>
          <P>
            Praesent vel hendrerit risus, nec sagittis enim. Cras nec lobortis
            justo. Praesent accumsan turpis scelerisque, fermentum metus sed,
            volutpat lectus. Mauris tempus eget ex sit amet vestibulum. Aliquam
            ut enim commodo, gravida odio in, venenatis est. Maecenas iaculis
            blandit tincidunt. Aenean faucibus luctus tellus, eu aliquet justo
            sollicitudin a. Donec eu convallis urna, quis porta quam. Vivamus
            rutrum et odio in varius. Sed tristique auctor mi, non vehicula est
            vehicula ut. Aliquam vestibulum, odio id finibus euismod, mauris
            ipsum iaculis massa, vel feugiat turpis sapien nec orci.
          </P>
        </article>
      </main>
    </div>
    <Default />
  </Layout>
);

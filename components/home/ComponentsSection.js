import {
  Nav,
  Sidebar,
  Section,
  Gallery,
  ButtonPrimary,
  ButtonSecondary,
  ButtonPrimaryPill,
  ButtonSecondaryPill,
  ButtonLinkPrimary,
  ButtonLinkSecondary,
  ButtonLinkPrimaryPill,
  ButtonLinkSecondaryPill,
  BannerGreen,
  BannerYellow,
  BannerRed,
  Code,
  NavBlockWrapper,
  Table,
  TH2,
  TD2,
  SidebarTitle,
  SidebarUL,
  SidebarLI,
  SidebarLink,
  LoginForm,
  H1,
  H2,
  H3,
  H4,
  H5,
  H6,
  P,
  BlockQuote,
  OL,
  UL,
  LI,
  Input,
  NavBlock,
} from "../../components";

export const ComponentsSection = props => (
  <Section>
    <H1>Components</H1>
    {/* @TODO: replace copy */}
    <P>
      Lorem ipsum dolor sit amet, consectetur adipiscing elit. Praesent
      convallis mollis nulla, molestie tempor velit consequat non. Integer quam
      ligula, consequat eget semper in, sodales nec mauris. Sed ultrices enim
      quis eros lobortis, semper condimentum eros sodales. Morbi iaculis lectus
      id dui convallis feugiat.
    </P>

    <div className="flex flex-wrap nl3 nr3">
      <Gallery title="Buttons">
        <div>
          <div className="mb2">
            <ButtonPrimary>Click me</ButtonPrimary>
          </div>
          <div className="mb2">
            <ButtonSecondary>Click me</ButtonSecondary>
          </div>
          <div className="mb2">
            <ButtonPrimaryPill>Click me</ButtonPrimaryPill>
          </div>
          <div>
            <ButtonSecondaryPill>Click me</ButtonSecondaryPill>
          </div>
        </div>
      </Gallery>

      <Gallery title="Link Buttons">
        <div>
          <div className="mb2">
            <ButtonLinkPrimary>Browse page</ButtonLinkPrimary>
          </div>
          <div className="mb2">
            <ButtonLinkSecondary>Browse page</ButtonLinkSecondary>
          </div>
          <div className="mb2">
            <ButtonLinkPrimaryPill>Browse page</ButtonLinkPrimaryPill>
          </div>
          <div>
            <ButtonLinkSecondaryPill>Browse page</ButtonLinkSecondaryPill>
          </div>
        </div>
      </Gallery>

      <Gallery title="Banners">
        <div className="w-100">
          <div>
            <BannerGreen>Email successfully delivered.</BannerGreen>
          </div>
          <div>
            <BannerYellow>Waiting for internet connection...</BannerYellow>
          </div>
          <div>
            <BannerRed>This email contains malicious attachment.</BannerRed>
          </div>
        </div>
      </Gallery>

      <Gallery title="Code Block">
        <div className="w-100">
          <Code>{`.tachyons {
  display: block;
}`}</Code>
        </div>
      </Gallery>

      <Gallery title="Table">
        <div className="w-100">
          <Table>
            <thead>
              <tr>
                <TH2>Column 1</TH2>
                <TH2>Column 2</TH2>
                <TH2>Column 3</TH2>
                <TH2>Column 4</TH2>
                <TH2>
                  <div className="tr">Column 5</div>
                </TH2>
              </tr>
            </thead>
            <tbody>
              <tr>
                <TD2>Red</TD2>
                <TD2>Green</TD2>
                <TD2>Blue</TD2>
                <TD2>Yellow</TD2>
                <TD2>
                  <div className="tr">001</div>
                </TD2>
              </tr>

              <tr>
                <TD2>Red</TD2>
                <TD2>Green</TD2>
                <TD2>Blue</TD2>
                <TD2>Yellow</TD2>
                <TD2>
                  <div className="tr">001</div>
                </TD2>
              </tr>

              <tr>
                <TD2>Red</TD2>
                <TD2>Green</TD2>
                <TD2>Blue</TD2>
                <TD2>Yellow</TD2>
                <TD2>
                  <div className="tr">001</div>
                </TD2>
              </tr>
            </tbody>
          </Table>
        </div>
      </Gallery>

      <Gallery title="Heading">
        <div>
          <H1>Heading 1</H1>
          <H2>Heading 2</H2>
          <H3>Heading 3</H3>
          <H4>Heading 4</H4>
          <H5>Heading 5</H5>
          <H6>Heading 6</H6>
        </div>
      </Gallery>

      <Gallery title="Sidebar">
        <div className="w-100">
          <Sidebar />
        </div>
      </Gallery>

      <Gallery title="Paragraph">
        <div>
          <H4>Introduction</H4>
          <P>
            You have power over your mind - not outside events. Realize this,
            and you will find strength.
          </P>
        </div>
      </Gallery>

      <Gallery title="Blockquote">
        <div>
          <BlockQuote>
            You have power over your mind - not outside events. Realize this,
            and you will find strength.
          </BlockQuote>
        </div>
      </Gallery>

      <Gallery title="Lists">
        <div>
          <OL>
            <LI>One</LI>
            <LI>Two</LI>
            <LI>Three</LI>
          </OL>

          <UL>
            <LI>One</LI>
            <LI>Two</LI>
            <LI>Three</LI>
          </UL>
        </div>
      </Gallery>

      <Gallery title="Forms">
        <div>
          <LoginForm />
        </div>
      </Gallery>

      <Gallery title="Input">
        <div className="w-100">
          <Input label="First Name" placeholder="John" />
          <Input label="Last Name" placeholder="Doe" />
        </div>
      </Gallery>

      <Gallery title="Nav Block">
        <div className="w-100">
          <NavBlockWrapper>
            <div className="flex flex-wrap nl3 nr3">
              <div className="w-50 ph3">
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
                </SidebarUL>
              </div>
              <div className="w-50 ph3">
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
                </SidebarUL>
              </div>
            </div>
          </NavBlockWrapper>
        </div>
      </Gallery>

      <Gallery title="Nav">
        <Nav />
      </Gallery>
    </div>
  </Section>
);

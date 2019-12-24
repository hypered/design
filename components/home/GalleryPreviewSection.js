import React from "react";

import {
  Nav,
  NavLink,
  Sidebar,
  Section,
  Gallery,
  Button,
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

export const GalleryPreviewSection = props => (
  <Section>
    <H2>Components</H2>
    {/* @TODO: replace copy */}
    <P>
      Lorem ipsum dolor sit amet, consectetur adipiscing elit. Praesent
      convallis mollis nulla, molestie tempor velit consequat non. Integer quam
      ligula, consequat eget semper in, sodales nec mauris. Sed ultrices enim
      quis eros lobortis, semper condimentum eros sodales. Morbi iaculis lectus
      id dui convallis feugiat.
    </P>

    <div className="flex flex-wrap nl3 nr3">
      <Gallery title="Input">
        <div className="w-100">
          <Input label="First Name" placeholder="John" />
          <Input label="Last Name" placeholder="Doe" />
        </div>
      </Gallery>

      <Gallery title="Buttons">
        <div>
          <div className="mb2">
            <Button variant="primary" size="normal">
              Click me
            </Button>
          </div>
          <div className="mb2">
            <Button variant="secondary" size="normal">
              Click me
            </Button>
          </div>
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
    </div>

    <div className="pt5">
      <div className="flex flex-column items-center">
        <P>And many more...</P>
        <a
          className="no-underline dib tc ph4 pv3 bg-black white ba bw1 b--black br-pill"
          href="components/"
        >
          <b>Go to Components Gallery</b>
        </a>
      </div>
    </div>
  </Section>
);

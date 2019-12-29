import React from "react";

import {
  Nav,
  NavLink,
  Sidebar,
  Section,
  Gallery,
  Button,
  ButtonLink,
  Banner,
  Radio,
  Checkbox,
  Dropdown,
  Option,
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
    <div className="flex flex-wrap nl3 nr3">
      <Gallery title="Buttons" href="../storybook/?path=/story/buttons--primary">
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

      <Gallery title="Link Buttons" href="../storybook/?path=/story/button-link--primary">
        <div>
          <div className="mb2">
            <ButtonLink variant="primary" href="#" size="normal">
              Click me
            </ButtonLink>
          </div>
          <div className="mb2">
            <ButtonLink variant="secondary" href="#" size="normal">
              Click me
            </ButtonLink>
          </div>
        </div>
      </Gallery>

      <Gallery title="Checkboxes" href="#">
        <div>
          <div className="mb2">
            <Checkbox
              type="checkbox"
              defaultChecked
              label="I agree to the terms and conditions"
            />
          </div>
          <div>
            <Checkbox
              type="checkbox"
              defaultChecked
              label="I agree to the terms and conditions"
              pill="true"
            />
          </div>
        </div>
      </Gallery>

      <Gallery title="Radio" href="#">
        <div>
          <div className="mb3">
            <Radio>
              <Checkbox pill="true" type="radio" label="Apple" value="apple" />
              <Checkbox
                pill="true"
                type="radio"
                label="Banana"
                value="banana"
              />
              <Checkbox
                pill="true"
                type="radio"
                label="Cherry"
                value="cherry"
              />
            </Radio>
          </div>
          <div>
            <Radio>
              <Checkbox type="radio" label="Apple" value="apple" />
              <Checkbox type="radio" label="Banana" value="banana" />
              <Checkbox type="radio" label="Cherry" value="cherry" />
            </Radio>
          </div>
        </div>
      </Gallery>

      <Gallery title="Dropdowns" href="#">
        <div>
          <div>
            <Dropdown>
              <Option>Select from dropdown</Option>
              <Option>Item One</Option>
              <Option>Item Two</Option>
              <Option>Item Three</Option>
            </Dropdown>
          </div>
        </div>
      </Gallery>

      <Gallery title="Banners" href="../storybook/?path=/story/banner--green">
        <div className="w-100">
          <div>
            <Banner color="green">Email successfully delivered.</Banner>
          </div>
          <div>
            <Banner color="yellow">Waiting for internet connection...</Banner>
          </div>
          <div>
            <Banner color="red">
              This email contains malicious attachment.
            </Banner>
          </div>
        </div>
      </Gallery>

      <Gallery title="Code Block" href="../storybook/?path=/story/code-block--code-block">
        <div className="w-100">
          <Code>{`.tachyons {
  display: block;
}`}</Code>
        </div>
      </Gallery>

      <Gallery title="Table" href="../storybook/?path=/story/table--default">
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

      <Gallery title="Heading" href="../storybook/?path=/story/typography--heading-1">
        <div>
          <H1>Heading 1</H1>
          <H2>Heading 2</H2>
          <H3>Heading 3</H3>
          <H4>Heading 4</H4>
          <H5>Heading 5</H5>
          <H6>Heading 6</H6>
        </div>
      </Gallery>

      <Gallery title="Sidebar" href="../storybook/?path=/story/sidebar--default">
        <div className="w-100">
          <nav>
            <SidebarTitle>Section 1</SidebarTitle>
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

            <SidebarTitle>Section 2</SidebarTitle>
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

            <SidebarTitle>Section 3</SidebarTitle>
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
          </nav>
        </div>
      </Gallery>

      <Gallery title="Paragraph" href="../storybook/?path=/story/typography--paragraph">
        <div>
          <H4>Introduction</H4>
          <P>
            You have power over your mind - not outside events. Realize this,
            and you will find strength.
          </P>
        </div>
      </Gallery>

      <Gallery title="Blockquote" href="../storybook/?path=/story/typography--paragraph">
        <div>
          <BlockQuote>
            You have power over your mind - not outside events. Realize this,
            and you will find strength.
          </BlockQuote>
        </div>
      </Gallery>

      <Gallery title="Lists" href="../storybook/?path=/story/list--ordered-list">
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

      <Gallery title="Forms" href="../storybook/?path=/story/form--login">
        <div className="w-100">
          <LoginForm />
        </div>
      </Gallery>

      <Gallery title="Input" href="../storybook/?path=/story/input--text">
        <div className="w-100">
          <Input label="First Name" placeholder="John" />
          <Input label="Last Name" placeholder="Doe" />
        </div>
      </Gallery>

      <Gallery title="Nav Block" href="../storybook/?path=/story/navigation-block--default">
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

      <Gallery title="Nav" href="../storybook/?path=/story/navigation--navigation">
        <Nav>
          <div>
            <NavLink active={"true"}>One</NavLink>
            <NavLink>Two</NavLink>
            <NavLink>Three</NavLink>
          </div>
        </Nav>
      </Gallery>
    </div>
  </Section>
);

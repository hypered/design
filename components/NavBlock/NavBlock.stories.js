import {
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
      <div className="w-100 w-25-m w-25-l">
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

      <div className="w-100 w-25-m w-25-l">
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

      <div className="w-100 w-25-m w-25-l">
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

      <div className="w-100 w-25-m w-25-l">
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

import React from 'react';
import { linkTo } from '@storybook/addon-links';
import {
  BareLayout,
  UL,
  OL,
  LI,
  H1,
  H2,
  H3,
  H4,
  P,
  Codeblock,
  BlockQuote,
  PullQuote,
  Divider,
  NavBlockWrapper,
  SidebarTitle,
  SidebarUL,
  SidebarLI,
  SidebarLink,
} from "../components";

export default {
  title: 'Hypered',
};

export const toStorybook = props => (
  <BareLayout>
    <main>
      <article className="mw7 cf">
        <div className="mb4">
          <H1>Hypered design system</H1>
        </div>

        <P>
          This is the Storybook-based component explorer for
          the <a href="/design/">Hypered design system</a>.
          The Git repository
          is <a href="https://github.com/hypered/design">on GitHub</a>.
        </P>
      </article>
    </main>
  </BareLayout>
);

toStorybook.storyName = 'Design system';

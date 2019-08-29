import React from "react";

function Nav() {
  return (
    <header class="">
      <nav class="flex align-items-center lh-copy">
        <a href="/" class="mr3 link black hover-blue">
          noteed.com
        </a>
        <a href="/blog/" class="mr3 link hover-blue black">
          blog
        </a>
        <a href="/projects/not-os/" class="mr3 link hover-blue black">
          not-os
        </a>
      </nav>
    </header>
  );
}

export { Nav };

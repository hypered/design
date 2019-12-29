#! /usr/bin/env bash

# Ugly query-replace to add the Entrypoint css and navigation to
# Haddock-generated HTML.

sed -i \
  -e 's@<link href="ocean.css" rel="stylesheet" type="text/css" title="Ocean" />@\n<link href="/static/css/ibm-plex.css" rel="stylesheet" />\n<link rel="stylesheet" type="text/css" href="/static/css/tachyons.min.v4.11.1.css" />\n<link rel="stylesheet" type="text/css" href="/static/css/style.css" />\n<link href="/static/css/haddock.css" rel="stylesheet" type="text/css" title="Ocean" />\n@' \
  -e 's@</head><body>@</head>\n<body class="ibm-plex-sans">\n<div class="ph3 mt2 mb4 f4 center main-mw">\n<ul class="list flex ma0 pa0 mt4 ep-nav">\n<li class="mr4"><a href="/design-system/">Home</a>\n<li class="mr4"><a href="/design-system/components/">Components</a>\n<li class="mr4"><a href="/design-system/storybook/">Storybook</a>\n<li class="mr4"><a href="/design-system/nix.html">Documentation</a>\n<li class="mr4"><a href="/design-system/haddock/">Haddock</a>\n</ul>\n</div>\n@'\
  -e 's@<div id="package-header">@<div id="package-header" class="ph3 mt2 mb4 f4 center main-mw">@' \
  -e 's@<div id="content">@<div id="content" class="ph3 mt2 mb4 f4 center main-mw">@' \
  $1

module Hypered.Html.Common where

import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H


--------------------------------------------------------------------------------
-- | This is a script to connect to the backend using websocket, and reload the
-- page when the connection is lost (and then successfully re-created). You can
-- thus add this element temporarily to a page when you're hacking at it using
-- something like ghcid.
-- This can be used as an element given to the
-- `Hypered.Html.Tachyons.document'` function.
autoReload :: Html
autoReload =
  H.preEscapedText
    "<script>\n\
  \function connect(isInitialConnection) {\n\
  \  // Create WebSocket connection.\n\
  \  var ws = new WebSocket('ws://' + location.host + '/ws');\n\
  \\n\
  \  // Connection opened\n\
  \  ws.onopen = function() {\n\
  \    ws.send('Hello server.');\n\
  \    if (isInitialConnection) {\n\
  \      console.log('autoreload: Initial connection.');\n\
  \    } else {\n\
  \      console.log('autoreload: Reconnected.');\n\
  \      location.reload();\n\
  \    };\n\
  \  };\n\
  \\n\
  \  // Listen for messages.\n\
  \  ws.onmessage = function(ev) {\n\
  \    console.log('autoreload: Message from server: ', ev.data);\n\
  \  };\n\
  \\n\
  \  // Trying to reconnect when the socket is closed.\n\
  \  ws.onclose = function(ev) {\n\
  \    console.log('autoreload: Socket closed. Trying to reconnect in 0.5 second.');\n\
  \    setTimeout(function() { connect(false); }, 500);\n\
  \  };\n\
  \\n\
  \  // Close the socker upon error.\n\
  \  ws.onerror = function(err) {\n\
  \    console.log('autoreload: Socket errored. Closing socket.');\n\
  \    ws.close();\n\
  \  };\n\
  \}\n\
  \\n\
  \connect(true);\n\
  \</script>\n"

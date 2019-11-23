-- This Pandoc Lua filter adds some Tachyons classes to headers (h1, h2, ...).
-- Example usage:
--
--   pandoc --lua-filter tachyons.lua lua.md 

function Header(elem)
  if (elem.level == 1) then
    elem.classes = {"f1", "lh-title", "mv2", "tracked-tight"}
  elseif (elem.level == 2) then
    elem.classes = {"f2", "lh-title", "mv2", "tracked-tight"}
  end
  return elem
end

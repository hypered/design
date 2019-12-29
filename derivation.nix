{ mkDerivation, base, blaze-builder, blaze-html, directory
, filepath, stdenv, text
}:
mkDerivation {
  pname = "design-system";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base blaze-builder blaze-html directory filepath text
  ];
  executableHaskellDepends = [ base blaze-html text ];
  description = "Design system for Hypered";
  license = stdenv.lib.licenses.bsd2;
  enableSeparateDocOutput = false;
}

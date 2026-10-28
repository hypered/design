{ nixpkgs ? import (import ../nix/sources.nix {}).nixpkgs {}
}:
let
  nodeModules = nixpkgs.importNpmLock.buildNodeModules {
    package = nixpkgs.lib.importJSON ./package.json;
    packageLock = nixpkgs.lib.importJSON ./package-lock.json;
    inherit (nixpkgs) nodejs;
  };
in rec
{
  inherit nodeModules;
  site = nixpkgs.stdenv.mkDerivation {
    name = "itcss";
    src = ../.;
    installPhase = ''
      # Make sure we don't use an already built dist/.
      rm -rf dist node_modules

      ln -sf ${nodeModules}/node_modules ./node_modules
      ./node_modules/gulp/bin/gulp.js build --gulpfile itcss/gulpfile.js --cwd itcss/
      mv itcss/dist $out
    '';
  };

  # Links node_modules/ in the current directory and puts its binaries (e.g.
  # gulp) on the PATH.
  shell = nixpkgs.mkShell {
    packages = [ nixpkgs.importNpmLock.hooks.linkNodeModulesHook nixpkgs.nodejs ];
    npmDeps = nodeModules;
  };
}

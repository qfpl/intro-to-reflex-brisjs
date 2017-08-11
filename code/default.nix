{ compiler   ? "ghcjs"
} :
let
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    reflex-platform = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-platform";
      rev = "b7c00b3574d0ef42974eda0f2812c794c7b5d4f3";
      sha256 = "1jfz17y2fq051caby4y4aslxrpvgwwa30ivfw0l5wn5pp5zlrpad";
    };
  };

  reflex-platform = import sources.reflex-platform {};
  pkgs = reflex-platform.nixpkgs.pkgs;

  adjust-for-ghcjs = drv: {
    executableToolDepends = [pkgs.closurecompiler pkgs.zopfli];
    doHaddock = false;
    postInstall = ''
      mkdir -p $out

      mkdir -p $out/css
      cp ./css/* $out/css/

      mkdir -p $out/js
      cp $out/bin/examples-exe.jsexe/all.js $out/js/examples.js

      cd $out/bin/examples-exe.jsexe
      closure-compiler all.js --compilation_level=ADVANCED_OPTIMIZATIONS --isolation_mode=IIFE --assume_function_wrapper --jscomp_off="*" --externs=all.js.externs > $out/js/examples.min.js
      rm -Rf $out/bin/examples-exe.jsexe
      rm -Rf $out/bin

      cd $out/js
      zopfli -i1000 examples.min.js

      rm -Rf $out/lib
      rm -Rf $out/nix-support
      rm -Rf $out/share
    '';
  };

  adjust-for-ghc = drv: {
    executableSystemDepends = [
      reflex-platform.${compiler}.ghcid
      (pkgs.callPackage (import ./tools/nix-tags-haskell) {})
    ];
  };

  adjust =
    if compiler == "ghcjs"
    then adjust-for-ghcjs
    else adjust-for-ghc;

  haskellPackages = reflex-platform.${compiler}.override {
    overrides = (self: super: {
      ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
      ghcWithPackages = self.ghc.withPackages;
    });
  };

  reflex-materials-code-base = haskellPackages.callPackage ./examples.nix {};
  reflex-materials-code = pkgs.haskell.lib.overrideCabal reflex-materials-code-base adjust;
in
  reflex-materials-code

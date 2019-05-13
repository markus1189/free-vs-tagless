{ pkgs ? import <nixpkgs> {}
, useCodecentricFont ? false
}: with pkgs;

let
  codecentricFont = import ./cc-font.nix { inherit pkgs; };
  ghcPackages = ghc.withPackages (p: with p;
    [
      bytestring
      dhall
      hlint
      HaTeX
      lens
      pandoc
      shake
      split
      text
      wreq
    ]
  );
  latexPackages = texlive.combine {
    inherit (texlive)
    adjustbox
    animate
    babel
    beamer
    chngcntr
    cleveref
    collectbox
    enumitem
    environ
    etoolbox
    excludeonly
    fancyvrb
    fvextra
    float
    framed
    ifplatform
    lineno
    listings
    mdframed
    media9
    microtype
    minted
    needspace
    ocgx2
    pgf
    scheme-medium
    smartdiagram
    tcolorbox
    textpos
    todonotes
    trimspaces
    upquote
    xcolor
    xstring
    ;
  };
  pyPackages = with pythonPackages; [ pygments ];
in
  runCommand "slides-build-input" {
    FONTCONFIG_FILE = makeFontsConf { fontDirectories = [ google-fonts ubuntu_font_family ] ++ (if useCodecentricFont then [codecentricFont] else []); };

    buildInputs = [
      coreutils
      ditaa
      eject
      ghcPackages
      graphviz
      imagemagick
      latexPackages
      sbt
      scalafmt
      scala
      unzip
      which
    ] ++ pyPackages;

    USE_CC_FONT = lib.boolToString useCodecentricFont;
  } ""

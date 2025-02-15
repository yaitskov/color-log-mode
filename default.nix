{ pkgs ? import <nixpkgs> {}
}:
with pkgs ;
let
  e = pkgs.emacs29;
  ert-scope = import (fetchFromGitHub { owner = "yaitskov";
                                        repo = "ert-scope";
                                        rev = "master";
                                        hash = "sha256-xb6h6ZJdx7QZf5cnQHw3mWcGACDatCYvlwKKevzRhGM=";
                                      }) {} ; # /home/dan/pro/my-emacs/ert-scope {};
in
e.pkgs.trivialBuild {
  pname = "color-log-mode";
  version = "0.0.1";

  src = ./.;

  packageRequires = [ e.pkgs.ert-async e.pkgs.f ert-scope ];
  nativeBuildInputs = [ git ];

  buildPhase = ''
    runHook preBuild

    echo BUILDING
    emacs -l package -f package-initialize -L lisp --batch -f batch-byte-compile lisp/*.el

    pushd test
    make clean
    make
    popd

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    LISPDIR=$out/share/emacs/site-lisp
    pushd lisp
    install -d $LISPDIR
    install *.el *.elc $LISPDIR
    popd

    runHook postInstall
  '';

  meta = {
    description = "...";
    license = lib.licenses.gpl3;
    platforms = lib.platforms.all;
  };
}

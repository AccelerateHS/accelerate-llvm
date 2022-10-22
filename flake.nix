{
  description = "LLVM backends for the Accelerate array language";
  nixConfig = {
    bash-prompt = "\\e[34;1maccelerate-llvm-devShell ~ \\e[0m";

    allow-import-from-derivation = true;

    substituters = [
      "https://cache.nixos.org" # nixos cache
      "https://hydra.iohk.io" # iog hydra cache
      "https://iohk.cachix.org" # iog cachix cache
    ];

    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" # nixos pubkey
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" # iog hydra pubkey
      "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo=" # iog cachix pubkey
    ];
  };

  inputs = {
    accelerate.url = "github:mangoiv/accelerate/mangoiv/switch-to-haskell-nix";
    nixpkgs.follows = "accelerate/nixpkgs";
    nixpkgs-upstream.follows = "accelerate/nixpkgs-upstream";
    haskell-nix.follows = "accelerate/haskell-nix";
    # haskell-nix.url = "github:input-output-hk/haskell.nix";
    pre-commit-hooks.follows = "accelerate/pre-commit-hooks";
  };

  outputs = {
    self,
    accelerate,
    haskell-nix,
    nixpkgs,
    nixpkgs-upstream,
    pre-commit-hooks,
  }:
    with nixpkgs.lib; let
      # we steal some of the function from the accelerate flake as that reduces boilerplate
      # and increases consistency
      inherit
        (accelerate)
        mkFlakeAttrsFor
        supportedghcs
        flattenAttrs
        ghcVer
        toolsForghc
        ;

      pkgsFor = system:
        import nixpkgs {
          inherit system;
          overlays = [haskell-nix.overlay];
        };

      # the reason why we have a plainPkgs is that the nixpkgs version used by IOG's haskell.nix is sometimes out of date
      # and the caching of tools is worse than on nixpkgs due to the use of their haskell-nix overlay
      plainpkgsFor = system: import nixpkgs-upstream {inherit system;};

      # We support a bunch of systems and ghc versions,
      # this is what the flakes provides outputs for
      # If you want to run nix flake show, make sure this is a singleton list that only contains your system,
      # because IFD will not succeed to build for other specified systems
      supportedsystems = ["x86_64-linux"];
      # supportedsystems = systems.flakeExposed;

      # unholy function that removes the symlinks that will
      # cause trouble in the nix derivation by replacing them with the original file
      unSymlink = sys: p: let
        pkgs = pkgsFor sys;
      in
        pkgs.runCommandLocal "unSymLink" {}
        ''
          mkdir -p $out
          cp -r ${p}/* $out
          chmod -R +w $out

          f=src/Language/Haskell/TH/Extra.hs
          cp --remove-destination $out/accelerate-llvm/$f $out/accelerate-llvm-native/$f
          cp --remove-destination $out/accelerate-llvm/$f $out/accelerate-llvm-ptx/$f

          cp --remove-destination $out/LICENSE $out/accelerate-llvm
          cp --remove-destination $out/LICENSE $out/accelerate-llvm-native
          cp --remove-destination $out/LICENSE $out/accelerate-llvm-ptx

          cp --remove-destination $out/README.md $out/accelerate-llvm
          cp --remove-destination $out/README.md $out/accelerate-llvm-native
          cp --remove-destination $out/README.md $out/accelerate-llvm-ptx
        '';

      perSystem = genAttrs supportedsystems;

      # utility function that, passed a ghc version in the list format
      # and a system name returns a pre-commit-check attrset with a shellHook
      # and a formatCheck that can be run
      precommitcheckForghc = ghcversion: system:
        pre-commit-hooks.lib.${system}.run
        {
          src = ./.;
          settings = {
            ormolu.defaultExtensions = [];
          };

          hooks = {
            cabal-fmt.enable = true;
            fourmolu.enable = false;
            hlint.enable = false;
            alejandra.enable = true;
            statix.enable = true;
            shellcheck.enable = true;
          };

          tools = {inherit (toolsForghc ghcversion system) fourmolu hlint;};
        };

      # builds, given a ghc version in the list format and a system name, a
      # haskell.nix stackProject that contains all accelerate libraries and executables
      # the resolver is chosen based on the ghcVersion passed, i.e. if you want a specific
      # ghc version, you need the appropriate resolver
      projectForghc = ghcversion: system: let
        pkgs = pkgsFor system;
        plainpkgs = plainpkgsFor system;
        gver = ghcVer ghcversion;
        tools = toolsForghc ghcversion system pkgsFor;
        inherit (gver) compiler-nix-name;
        moduleFixes = [
          (
            {pkgs, ...}: {
              packages = {
              };
            }
          )
        ];
      in
        pkgs.haskell-nix.stackProject' {
          src = unSymlink system ./.;
          inherit compiler-nix-name;
          stackYaml = "stack-${gver.stack}.yaml";
          # modules = moduleFixes;
          shell = {
            inherit (precommitcheckForghc ghcversion system) shellHook;
            withHoogle = true;
            exactDeps = true;

            nativeBuildInputs = [
              plainpkgs.alejandra
              plainpkgs.cabal-install
              plainpkgs.stack
              plainpkgs.fd
              plainpkgs.ripgrep

              plainpkgs.haskellPackages.apply-refact
              plainpkgs.haskellPackages.cabal-fmt

              tools.fourmolu
              tools.haskell-language-server
              tools.hlint
            ];
          };
        };

      # a tooling shell that provides all the necessary stuff to do
      # formatting and quick adjustments, it does not provide a full dev env
      toolingShellFor = ghcversion: system: let
        plainpkgs = plainpkgsFor system;
        tools = toolsForghc ghcversion system plainpkgsFor;
      in
        plainpkgs.mkShell {
          inherit (precommitcheckForghc ghcversion system) shellHook;
          nativeBuildInputs = [
            plainpkgs.cabal-install
            plainpkgs.stack
            plainpkgs.fd
            plainpkgs.ripgrep

            plainpkgs.alejandra
            plainpkgs.haskellPackages.apply-refact
            plainpkgs.haskellPackages.cabal-fmt

            tools.fourmolu
            tools.haskell-language-server
            tools.hlint
          ];
        };

      accelerateFlakes = mkFlakeAttrsFor supportedghcs projectForghc perSystem;
    in {
      inherit supportedsystems unSymlink supportedghcs flattenAttrs ghcVer mkFlakeAttrsFor toolingShellFor toolsForghc;
      # FIXME: checks have to be fixed; checks pass with stack --nix test but not with cabal test
      inherit (accelerateFlakes) flakes projects packages checks;

      pkgs = perSystem pkgsFor;
      plainpkgs = perSystem plainpkgsFor;

      devShells = perSystem (sys:
        accelerateFlakes.devShells.${sys}
        // rec {
          # the default shell is the tooling shell as it loads fastest
          default = tooling;
          tooling = toolingShellFor [9 2 4] sys;
        });

      formatter = perSystem (system: self.pkgs.${system}.alejandra);
    };
}

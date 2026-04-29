{ inputs, lib, ... }:
{
  nixpkgs.overlays = [
    (self: super: {
      btop = super.btop.override { cudaSupport = true; };
      llama-cpp = (super.llama-cpp.override { cudaSupport = true; }).overrideAttrs { 
        src = inputs.llama-cpp; 
        npmDepsHash = "sha256-iYJB0z2YHG8OzEA9EwHUZrDa5obr5m2sbnIH+of28o0=";
      };
      sage = super.sage.override { requireSageTests = false; };
    })
  ];

  flake.modules.nixos.base = { pkgs, lib, ... }: {
    nix.settings.substituters = [
      "https://nix-community.cachix.org"
    ];

    nix.settings.trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];

    nix.settings.experimental-features = [
      "nix-command"
      "flakes"
    ];
  };
}

{ inputs, ... }:
{
  flake.modules.nixos.cuda = { pkgs, lib, ... }: {
    nixpkgs.overlays = [
      (self: super: {
        btop = super.btop.override { cudaSupport = true; };
        llama-cpp = super.llama-cpp.override { cudaSupport = true; };
      })
    ];
  };
}

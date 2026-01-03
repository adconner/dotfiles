{inputs, ...}: {
  perSystem = systemInputs@{pkgs, ... }: {
    packages.neovim = (inputs.nvf.lib.neovimConfiguration {
      inherit pkgs;
      modules = [ (import ./config.nix systemInputs) ];
    }).neovim;
  };
}

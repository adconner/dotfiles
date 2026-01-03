{inputs, ...}: {
  perSystem = systemInputs@{pkgs, self', ... }: {
    packages.default = self'.packages.neovim;
    packages.neovim = (inputs.nvf.lib.neovimConfiguration {
      inherit pkgs;
      modules = [ (import ./config.nix systemInputs) ];
    }).neovim;
  };
}

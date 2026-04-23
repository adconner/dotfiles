{inputs, lib, ...}: {
  perSystem = {pkgs, self', lib, ... }: {
    packages.default = self'.packages.neovim;
    packages.neovim = (inputs.nvf.lib.neovimConfiguration { 
      inherit pkgs; 
      modules = [ (import ./_config.nix { inherit pkgs inputs lib; }) ]; 
    }).neovim;
  };
}

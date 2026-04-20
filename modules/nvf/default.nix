{inputs, ...}: {
  perSystem = {pkgs, self', ... }: {
    packages.default = self'.packages.neovim;
    packages.neovim = (inputs.nvf.lib.neovimConfiguration { 
      inherit pkgs; 
      modules = [ (import ./_config.nix { inherit pkgs inputs; }) ]; 
    }).neovim;
  };
}

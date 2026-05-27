{ inputs, lib, ... }:
{
  flake.modules.homeManager.base = { pkgs, ... }: {
    home.packages = with pkgs; [ 
      # Use the neovim package from nvf
      (inputs.nvf.lib.neovimConfiguration { 
        inherit pkgs; 
        modules = [ (import ./nvf/_config.nix { inherit pkgs inputs lib; }) ]; 
      }).neovim
      opencode
      pi-coding-agent
      atool
      gzip
      bzip3
      ncompress
      zip
      unzip
      xz
      lzop
      p7zip
      # rar

      wget
      clang
      dfc
      tree
      ripgrep
      fzf
      fd
      jujutsu
      # sage
      pyright
      viddy
      mosh
      devenv
      nvitop
      nix-search-cli
      nnn
      pulsemixer
      rclone
      texliveFull
    ];
  };

  flake.modules.homeManager.gui = { pkgs, ... }: {
    home.packages = with pkgs; [
      sxiv
      pianobar
      wl-clipboard
      llama-cpp
      btop
    ];
  };

  # nixpkgs.overlays = [
  #   (self: super: {
  #     sage = super.sage.override { requireSageTests = false; };
  #   })
  # ];
}

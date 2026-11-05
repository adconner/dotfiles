{ inputs, lib, ... }:
{
  flake.modules.homeManager.base = { pkgs, ... }: {
    imports = [ inputs.nvf.homeManagerModules.default ];

    # Use the neovim package from nvf
    programs.nvf = {
      enable = true;
      defaultEditor = true;
      settings.imports = [ ./nvf/_config.nix ];
    };

    home.packages = with pkgs; [ 
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
      sage
      pyright
      viddy
      mosh
      devenv
      nvitop
      nnn
      pulsemixer
      rclone
      texliveFull
      nix-search-cli
      hydra-check
      mdcat
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

  nixpkgs.overlays = [
    (self: super: {
      sage = super.sage.override { requireSageTests = false; };
    })
  ];
}

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
      dfc
      tree
      ripgrep
      fzf
      fd
      jujutsu
      viddy
      mosh
      nvitop
      nnn
      pulsemixer
      rclone
      nix-search-cli
      hydra-check
      mdcat
    ];
  };

  # Costly to build from source; each configuration decides whether to import it.
  flake.modules.homeManager.heavy = { pkgs, ... }: {
    home.packages = with pkgs; [
      pi-coding-agent
      clang
      sage
      pyright
      devenv
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

  nixpkgs.overlays = [
    (self: super: {
      sage = super.sage.override { requireSageTests = false; };
    })
  ];
}

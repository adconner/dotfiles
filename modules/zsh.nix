{ inputs, lib, config, ... }:
{
  flake.modules.nixos.base = { pkgs, lib, ... }:
  {
    programs.zsh = {
      enable = true;
      enableGlobalCompInit = false;
      promptInit = "";
    };
    users.users.${config.flake.meta.owner.username}.shell = pkgs.zsh;
  };

  flake.modules.homeManager.base = { pkgs, lib, ... }: let
    jjCompletion = pkgs.runCommand "jj-completion.zsh" {} ''
      COMPLETE=zsh ${pkgs.jujutsu}/bin/jj > "$out"
    '';
  in {
    programs.zsh = {
      enable = true;
      initContent = ''
        # Skip compaudit (nix store has fixed perms, audit is slow I/O on some mounts)
        zstyle ':grml:completion:compinit' arguments -C
        source ${pkgs.grml-zsh-config}/etc/zsh/zshrc
        source ${pkgs.fzf}/share/fzf/key-bindings.zsh
        fpath+=${inputs.zsh-jj}/functions
        zstyle ':vcs_info:*' enable jj
        alias cd="z"
        source ${jjCompletion}
      '';

      enableCompletion = false;
      autosuggestion.enable = true;
      syntaxHighlighting.enable = true;

      shellAliases = {
        vi = "nvim";
        vim = "nvim";
        ll = "ls -l";
        edit = "sudo -e";
      };

      history = {
        path = "$HOME/.zsh_history";
        size = 10000;
        ignoreAllDups = true;
      };
    };
  };
}

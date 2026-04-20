{ inputs, lib, config, ... }:
{
  flake.modules.nixos.base = { pkgs, lib, ... }:
  {
    programs.zsh.enable = true;
    users.users.${config.flake.meta.owner.username}.shell = pkgs.zsh;
  };

  flake.modules.homeManager.base = { pkgs, lib, ... }: {
    programs.zsh = {
      enable = true;
      initContent = ''
        source ${pkgs.grml-zsh-config}/etc/zsh/zshrc
        source ${pkgs.fzf}/share/fzf/key-bindings.zsh
        fpath+=${inputs.zsh-jj}/functions
        zstyle ':vcs_info:*' enable jj
        alias cd="z"
        source <(COMPLETE=zsh jj)
      '';

      enableCompletion = true;
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

{ lib, config, ... }:
{
  flake.modules.homeManager.base = { pkgs, lib, ... }:
  let
    owner = config.flake.meta.owner;
  in
  {
    programs.git = {
      enable = true;
      settings = {
        user.name = owner.name;
        user.email = owner.email;
      };
    };
    programs.yazi = {
      enable = true;
      enableZshIntegration = true;
      shellWrapperName = "y";
    };
    programs.htop.enable = true;
    programs.direnv.enable = true;
    programs.zoxide = {
      enable = true;
      enableZshIntegration = true;
    };
  };

  flake.modules.homeManager.gui = { pkgs, lib, ... }: {
    home.sessionVariables.TERMINAL = "ghostty";
    xdg.configFile."xdg-terminals.list".text = "com.mitchellh.ghostty.desktop\n";

    programs.ghostty = {
      enable = true;
      settings = {
        font-family = "JetBrainsMono Nerd Font";
        font-size = 12;
        window-decoration = "none";
        window-show-tab-bar = "never";
        # Ghostty has no switch to disable tabs entirely; remove their shortcut too.
        keybind = [ "ctrl+shift+t=unbind" ];
        bell-features = "no-system,no-audio,no-attention,no-title,no-border";
        confirm-close-surface = false;
      };
    };
    programs.zathura = {
      enable = true;
      mappings = {
        "d" = "scroll half-down";
        "u" = "scroll half-up";
        "D" = "toggle_page_mode";
      };
      options = {
        "highlight-transparency" = 0.1;
        synctex-editor-command = "nvim -v --not-a-term -T dumb -c \"VimtexInverseSearch %{line}:%{column} '%{input}'\"";
      };
    };
  };

}

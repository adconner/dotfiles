{
  flake.modules.homeManager.ghostty = {
    home.sessionVariables.TERMINAL = "ghostty";
    xdg.configFile."xdg-terminals.list".text = "com.mitchellh.ghostty.desktop\n";

    programs.ghostty = {
      enable = true;
      settings = {
        background = "#1f1f1f";
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
  };
}

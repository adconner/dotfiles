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
    programs.zoxide.enable = true;
    home.sessionVariables = {
      EDITOR = "nvim";
    };
  };

  flake.modules.homeManager.gui = { pkgs, lib, ... }: {
    programs.alacritty = {
      enable = true;
      settings = {
        font = {
          size = 12.0;
          normal = {
            family = "JetBrainsMono Nerd Font";
            style = "Regular";
          };
          bold = {
            family = "JetBrainsMono Nerd Font";
            style = "Bold";
          };
          italic = {
            family = "JetBrainsMono Nerd Font";
            style = "Italic";
          };
          bold_italic = {
            family = "JetBrainsMono Nerd Font";
            style = "Bold Italic";
          };
        };
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

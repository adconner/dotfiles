{ config, ... }:
{
  flake.modules.nixos.desktop = {
    programs.niri.enable = true;
    programs.noctalia = {
      enable = true;
      recommendedServices.enable = true;
      # Scope the shell to Niri, so it does not also start in COSMIC.
      systemd = {
        enable = true;
        target = "niri.service";
      };
    };
    services.displayManager.defaultSession = "cosmic";

    home-manager.users.${config.flake.meta.owner.username}.imports = [
      config.flake.modules.homeManager.niri
    ];
  };

  flake.modules.homeManager.niri = { pkgs, ... }: {
    home.packages = [ pkgs.xwayland-satellite ];
    xdg.configFile."niri/config.kdl".source = ./niri/_config.kdl;

    programs.noctalia = {
      enable = true;
      # NixOS manages the service and ties it to the Niri session.
      systemd.enable = false;
      # Menu settings were exported from ~/.local/state/noctalia/settings.toml.
      settings = let
        wallpaper = "${./niri/_forestbnw.jpg}";
      in pkgs.lib.recursiveUpdate
        (builtins.fromTOML (builtins.readFile ./niri/_noctalia.toml))
        {
          wallpaper.default.path = wallpaper;
          wallpaper.last.path = wallpaper;
          wallpaper.monitors.DP-1.path = wallpaper;
        };
    };
  };
}

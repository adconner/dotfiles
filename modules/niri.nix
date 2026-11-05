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
      settings = {
        shell = {
          font_family = "JetBrainsMono Nerd Font";
          animation.enabled = false;
          polkit_agent = true;
        };
        bar.main = {
          margin_ends = 8;
          compositor_blur = false;
          start = [ "launcher" "workspaces" ];
          center = [ "clock" ];
          end = [ "tray" "notifications" "network" "volume" "brightness" "battery" "control-center" "session" ];
        };
        backdrop.enabled = false;
        wallpaper.enabled = false;
        dock.enabled = false;
        system.monitor.enabled = false;
        lockscreen = {
          enabled = false;
          transition = [ ];
        };
        idle.behavior = {
          lock.enabled = false;
          screen-off.enabled = false;
        };
      };
    };
  };
}

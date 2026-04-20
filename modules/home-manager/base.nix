{ config, ... }:
{
  flake.modules.homeManager.base = { pkgs, lib, ... }: {
    home.username = config.flake.meta.owner.username;
    home.homeDirectory = "/home/${config.flake.meta.owner.username}";
    programs.home-manager.enable = true;
    systemd.user.startServices = "sd-switch";
    home.stateVersion = "26.05";
  };
}

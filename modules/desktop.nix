{ lib, ... }:
{
  flake.modules.nixos.desktop = { pkgs, lib, ... }: {
    services.xserver.enable = true;
    services.xserver.videoDrivers = [ "nvidia" ];
    hardware.nvidia.open = true;
    hardware.graphics.enable = true;
    services.displayManager.cosmic-greeter.enable = true;
    services.desktopManager.cosmic.enable = true;
    services.playerctld.enable = true;
    services.xserver.xkb = {
      layout = "us";
      variant = "dvorak";
      options = "ctrl:nocaps";
    };
    programs.firefox.enable = true;
    programs.steam.enable = true;
    programs.nix-ld.enable = true;
  };
}

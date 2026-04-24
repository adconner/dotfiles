{ lib, ... }:
{
  flake.modules.nixos.base = { pkgs, lib, ... }: {
    services.openssh = {
      enable = true;
      settings = {
        PasswordAuthentication = false;
      };
    };
    zramSwap.enable = true;
    services.earlyoom = {
      enable = true;
      freeSwapThreshold = 5;
      freeMemThreshold = 5;
    };
  };
}

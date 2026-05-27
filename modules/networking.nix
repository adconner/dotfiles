{ lib, ... }:
{
  flake.modules.nixos.base = { pkgs, lib, ... }: {
    networking.networkmanager.enable = true;
    # networking.firewall.enable = false;
  };
}

{ lib, ... }:
{
  flake.modules.nixos.base = { pkgs, lib, ... }: {
    networking.hostName = "oscar";
    networking.networkmanager.enable = true;
    # networking.firewall.enable = false;
  };
}

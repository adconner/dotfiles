{ lib, ... }:
{
  flake.modules.nixos.base = { pkgs, lib, ... }: {
    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;
    boot.kernelPackages = pkgs.linuxPackages_latest;
    boot.tmp.useTmpfs = true;
  };
}

{ lib, ... }:
{
  flake.modules.nixos.base = { pkgs, lib, ... }: {
    time.timeZone = "America/New_York";
    i18n.defaultLocale = "en_US.UTF-8";
    i18n.extraLocaleSettings = {
      LC_ALL = "en_US.UTF-8";
    };
    console.keyMap = "dvorak";
  };
}

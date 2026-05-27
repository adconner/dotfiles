{ config, ... }:
{
  flake = {
    modules = {
      nixos.base = { pkgs, lib, ... }:
      {
        users.users.${config.flake.meta.owner.username} = {
          isNormalUser = true;
          initialPassword = "password";
          description = config.flake.meta.owner.name;
          extraGroups = [ "networkmanager" "wheel" ];
        };
      };
    };
  };
}

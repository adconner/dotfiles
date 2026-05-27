{ config, inputs, ... }:
{
  config.configurations.nixos.oscar = {
    module = { pkgs, lib, ... }: {
      imports = [
        ./_hardware-configuration.nix
        config.flake.modules.nixos.base
        config.flake.modules.nixos.desktop
        config.flake.modules.nixos.cuda
      ];

      networking.hostName = "oscar";

      system.stateVersion = "25.05";
    };
  };
  config.flake.nixosConfigurations.default = config.flake.nixosConfigurations.oscar;
}

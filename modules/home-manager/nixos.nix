{ config, inputs, ... }:
{
  flake.modules.nixos = {
    base = {
      imports = [ inputs.home-manager.nixosModules.home-manager ];

      home-manager = {
        useGlobalPkgs = true;
        users.${config.flake.meta.owner.username}.imports = [
          config.flake.modules.homeManager.base
          config.flake.modules.homeManager.gui
        ];
      };
    };
  };
}

{ config, inputs, ... }:
{
  config.configurations.nixos.herbie = {
    module = { pkgs, lib, ... }: {
      imports = [
        inputs.disko.nixosModules.default
        ./_disko.nix
        inputs.nixos-facter-modules.nixosModules.facter
        { facter.reportPath = ./facter.json; }
        config.flake.modules.nixos.base
        config.flake.modules.nixos.desktop
      ];

      networking.hostName = "herbie";

      # Override nvidia from desktop module — herbie has no dedicated GPU
      services.xserver.videoDrivers = lib.mkForce [ "amdgpu" ];
      hardware.nvidia.open = lib.mkForce false;

      system.stateVersion = "25.05";
    };
  };
}

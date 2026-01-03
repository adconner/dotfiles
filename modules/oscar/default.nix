{ inputs, ... }: {
  flake.nixosConfigurations.oscar = inputs.nixpkgs.lib.nixosSystem {
    specialArgs = { inherit inputs; };
    modules = [ ./configuration.nix ];
  };
}

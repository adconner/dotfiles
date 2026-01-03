{ inputs, self', ... }: {
  flake.nixosConfigurations.default = self'.flake.nixosConfigurations.oscar;
  flake.nixosConfigurations.oscar = inputs.nixpkgs.lib.nixosSystem {
    specialArgs = { inherit inputs; };
    modules = [ ./configuration.nix ];
  };
}

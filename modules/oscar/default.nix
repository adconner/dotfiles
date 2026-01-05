{ inputs, self, withSystem, ... }: {
  flake.nixosConfigurations.default = self.flake.nixosConfigurations.oscar;
  flake.nixosConfigurations.oscar = withSystem "x86_64-linux" ({self', ...} :
    inputs.nixpkgs.lib.nixosSystem {
    specialArgs = {
      inherit inputs;
      inherit self';
    };
    modules = [ ./configuration.nix ];
  });
}

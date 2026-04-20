{
  lib,
  config,
  inputs,
  withSystem,
  ...
}:
{
  options.nixpkgs = {
    overlays = lib.mkOption {
      type = lib.types.listOf lib.types.unspecified;
      default = [ ];
    };
  };

  config = {
    perSystem = {
      system,
      ...
    }: {
      _module.args.pkgs = import inputs.nixpkgs {
        inherit system;
        config = { allowUnfree = true; };
        overlays = config.nixpkgs.overlays;
      };
    };

    flake.modules.nixos.base = _nixosArgs: {
      nixpkgs = {
        pkgs = withSystem "x86_64-linux" (ps: ps.pkgs);
        hostPlatform = "x86_64-linux";
      };
    };
  };
}

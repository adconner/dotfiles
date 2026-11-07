{ ... }:
{
  flake.modules.nixos.desktop = { pkgs, ... }: {
    programs.umbriel.enable = true;

    home-manager.sharedModules = [
      {
        xdg.configFile."umbriel/config.toml".source =
          pkgs.runCommand "umbriel-config.toml" { } ''
            ${pkgs.umbriel}/bin/umbriel config validate -c ${./umbriel/_config.toml}
            cp ${./umbriel/_config.toml} $out
          '';
      }
    ];
  };
}

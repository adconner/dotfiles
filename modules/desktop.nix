{ lib, ... }:
{
  flake.modules.nixos.desktop = { pkgs, lib, ... }: {
    services.xserver.enable = true;
    services.xserver.videoDrivers = [ "nvidia" ];
    hardware.nvidia.open = true;
    hardware.graphics.enable = true;
    services.displayManager.cosmic-greeter.enable = true;
    services.desktopManager.cosmic.enable = true;
    home-manager.sharedModules = [
      ({ config, lib, pkgs, ... }: {
        # Keep COSMIC's config writable; restore our shortcuts on activation.
        home.activation.cosmicDesktopDefaults = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          run ${pkgs.writeShellScript "cosmic-desktop-defaults" ''
            set -eu
            shortcuts=${lib.escapeShellArg "${config.xdg.configHome}/cosmic/com.system76.CosmicSettings.Shortcuts/v1"}
            mkdir -p "$shortcuts"
            if [ -f "$shortcuts/system_actions" ]; then
              ${pkgs.gnused}/bin/sed -i '/Terminal:/d; /}/i\    Terminal: "ghostty",' "$shortcuts/system_actions"
            else
              printf '{\n    Terminal: "ghostty",\n}\n' > "$shortcuts/system_actions"
            fi
            ${pkgs.coreutils}/bin/install -m 0644 ${./cosmic/_shortcuts.ron} "$shortcuts/custom"
            export XDG_CONFIG_HOME=${lib.escapeShellArg config.xdg.configHome}
            ${pkgs.xdg-utils}/bin/xdg-mime default com.mitchellh.ghostty.desktop application/x-terminal-emulator
            ${pkgs.xdg-utils}/bin/xdg-mime default com.mitchellh.ghostty.desktop x-scheme-handler/terminal
          ''}
        '';
      })
    ];
    services.playerctld.enable = true;
    services.xserver.xkb = {
      layout = "us";
      variant = "dvorak";
      options = "ctrl:nocaps";
    };
    programs.firefox.enable = true;
    programs.steam.enable = true;
    programs.nix-ld.enable = true;
  };
}

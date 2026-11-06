{ inputs, config, ... }:
let
  # The work desktop can't run sandboxed Nix builds, so everything is built on
  # the work dev box into this store and served to the work desktop as a
  # signed binary-cache directory (synced with rsync).
  root = "/home/aconner/nix-desktop";

  # Everything here is built from source (cache.nixos.org only serves
  # /nix/store), so it needs fixes for tests that only pass on the build farm.
  pkgs = import inputs.nixpkgs {
    system = "x86_64-linux";
    config.allowUnfree = true;
    overlays = config.nixpkgs.overlays ++ [
      (final: prev: {
        # `make check` compares zones "identical from now on" against the real
        # date; tzdata 2026d/2026e fail it from 2026-11 (Vancouver joins Phoenix).
        tzdata = prev.tzdata.overrideAttrs { doCheck = false; };
        # sslapitest's test_ktls depends on the build host's kernel TLS support.
        openssl = prev.openssl.overrideAttrs { doCheck = false; };
        # pipe_write_cancel_ipc expects its writes to fill the socket buffer, which
        # depends on the build host's default buffer size.
        libuv = prev.libuv.overrideAttrs (old: {
          postPatch = old.postPatch + ''
            sed '/pipe_write_cancel_ipc/d' -i test/test-list.h
          '';
        });
        # Restoring the nodump flag depends on the build directory's filesystem.
        libarchive = prev.libarchive.overrideAttrs (old: {
          postPatch = old.postPatch + ''
            substituteInPlace Makefile.am --replace-fail tar/test/test_option_fflags.c ""
            rm tar/test/test_option_fflags.c
          '';
        });
        # udevadm resolves paths with statx(STATX_MNT_ID), which the build host's
        # kernel may lack, so no `udevadm verify` at build time.
        systemdMinimal = prev.systemdMinimal.overrideAttrs { doInstallCheck = false; };
        udevCheckHook = final.makeSetupHook { name = "udev-check-hook"; } (
          final.writeText "no-udev-check.sh" ""
        );
        # These chgrp to a group from os.getgroups(), which fails for supplementary
        # groups that are unmapped in an unprivileged build sandbox.
        rsync = prev.rsync.overrideAttrs (old: {
          preCheck = old.preCheck + ''
            export RSYNC_EXCLUDE="$RSYNC_EXCLUDE,chgrp,daemon-groupmap-wild,ownership-depth"
          '';
        });
        # nix's test gate: readLinkAt makes symlinks longer than some filesystems
        # allow. The perl bindings' own test assumes /nix/store.
        nix =
          (prev.nix.overrideScope (
            _: scope: {
              nix-perl-bindings = scope.nix-perl-bindings.overrideAttrs { doCheck = false; };
            }
          )).overrideAttrs
            { doCheck = false; };
        # jj uses gnupg only in its test suite; gnupg's TPM backend drags in
        # iproute2 -> rdma-core -> pandoc -> GHC.
        jujutsu = prev.jujutsu.override {
          gnupg = final.gnupg.override { withTpm2Tss = false; };
        };
        # Ghostty's man pages need pandoc (GHC); its build always emits docs as a
        # system package unless told not to. postFixup still links $man/share/man.
        # Its GStreamer plugins only serve the audio bell, which plays through
        # gtk4's media backend (disabled below).
        ghostty = prev.ghostty.overrideAttrs (old: {
          nativeBuildInputs = final.lib.remove final.pandoc old.nativeBuildInputs;
          buildInputs = final.lib.subtractLists (with final.gst_all_1; [
            gstreamer
            gst-plugins-base
            gst-plugins-good
          ]) old.buildInputs;
          zigBuildFlags = old.zigBuildFlags ++ [ "-Demit-docs=false" ];
          postInstall = old.postInstall + ''
            mkdir -p $man/share/man
          '';
        });
        # gtk4's GStreamer media backend (video widgets, unused by Ghostty) pulls
        # in ffmpeg, pipewire and more.
        gtk4 = prev.gtk4.overrideAttrs (old: {
          buildInputs = final.lib.subtractLists [
            final.gst_all_1.gst-plugins-base
            final.gst_all_1.gst-plugins-bad
          ] old.buildInputs;
          mesonFlags = old.mesonFlags ++ [ "-Dmedia-gstreamer=disabled" ];
        });
        # wayland's docs need graphviz.
        wayland = prev.wayland.override { withDocumentation = false; };
      })
    ];
  };
in
{
  flake.homeConfigurations.aconner-work-desktop =
      inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          # Not homeManager.heavy: too costly to build from source for this store.
          config.flake.modules.homeManager.base
          {
            home.username = "aconner";
            home.homeDirectory = "/home/aconner";
            targets.genericLinux.enable = true;
            # Its setup needs root and pulls Mesa + LLVM into the build.
            targets.genericLinux.gpu.enable = false;
            # Use the host's systemctl instead of building nixpkgs' systemd.
            systemd.user.systemctlPath = "/usr/bin/systemctl";

            programs.nvf.settings.imports = [ ../nvf/_work-desktop.nix ];
            home.packages = [
              # home-manager's CLI runs `nix` from PATH; use the one matching this store.
              pkgs.nix
            ];
            # yazi's default optional deps minus ffmpeg-headless (video previews),
            # whose build needs clang.
            programs.yazi.package = pkgs.yazi.override {
              optionalDeps = with pkgs; [
                jq poppler-utils _7zz fd ripgrep fzf zoxide imagemagick chafa resvg
              ];
            };

            nix.package = pkgs.nix;
            nix.settings = {
              experimental-features = [ "nix-command" "flakes" ];
              store = "local?store=${root}/store&state=${root}/var/nix&log=${root}/var/log/nix";
              # ?store= is required: a substituter is assumed to be for /nix/store otherwise.
              substituters = [ "file://${root}/cache?store=${root}/store" ];
              trusted-public-keys = [ "aconner-nix-desktop-1:utwG6ie9KSIBqcOJi4j3+1T4qJlU8u+B/j8YcDqebWw=" ];
              # Take everything from the cache (even allowSubstitutes = false
              # derivations) and never build locally.
              always-allow-substitutes = true;
              max-jobs = 0;
              # Without network access nix turns substitution off unless it is set
              # explicitly, and the cache is local.
              substitute = true;
            };
          }
          (
            { config, ... }:
            {
              # Nix-built OpenGL programs can't load the host's GPU drivers; nixGL
              # wraps them with a driver from this store. The display GPU runs
              # NVIDIA's driver, which Mesa has no support for, so the wrapper bundles
              # NVIDIA's user-space driver. It must match the host's kernel module
              # (/proc/driver/nvidia/version): update the version and hash with it.
              targets.genericLinux.nixGL = {
                packages =
                  let
                    nixgl = pkgs.callPackage "${inputs.nixgl}/nixGL.nix" {
                      nvidiaVersion = "595.99.02";
                      nvidiaHash = "sha256-6HR3lYv3YwcFSTJL1a1slI66btIQ5EAFs+/4SUD24ew=";
                      enable32bits = false;
                      intel-media-driver = null;
                      # nixGL passes `kernel` to nixpkgs' NVIDIA package, which no longer
                      # takes it. Its bundled 32-bit libraries would need an i686 toolchain.
                      linuxPackages.nvidia_x11 = pkgs.lib.makeOverridable (
                        {
                          kernel ? null,
                          libsOnly ? false,
                        }:
                        pkgs.linuxPackages.nvidia_x11.override {
                          inherit libsOnly;
                          disable32Bit = true;
                        }
                      ) { };
                    };
                    # Its wrapper scripts are linted with shellcheck, which needs GHC.
                    noLint = wrapper: wrapper.overrideAttrs { checkPhase = ""; };
                  in
                  nixgl
                  // {
                    nixGLIntel = noLint nixgl.nixGLIntel;
                    nixGLNvidia = noLint nixgl.nixGLNvidia;
                  };
                defaultWrapper = "nvidia";
              };
              programs.ghostty = {
                enable = true;
                package = config.lib.nixGL.wrap pkgs.ghostty;
                # Start zsh rather than the login shell from passwd, which can't be
                # changed here, and make it $SHELL for everything started inside
                # (tmux, nvim). The profile path survives garbage collection of old
                # generations, which long-running tmux servers may outlive.
                settings =
                  let
                    zsh = "${config.home.profileDirectory}/bin/zsh";
                  in
                  {
                    command = zsh;
                    env = [ "SHELL=${zsh}" ];
                  };
              };
            }
          )
        ];
      };
}

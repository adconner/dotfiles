{ inputs, config, withSystem, ... }:
{
  flake.homeConfigurations.aconner =
    withSystem "x86_64-linux" ({ pkgs, ... }:
      inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          config.flake.modules.homeManager.base
          config.flake.modules.homeManager.heavy
          {
            home.username = "aconner";
            home.homeDirectory = "/home/aconner";

            home.sessionVariables.NIX_SSL_CERT_FILE = "/etc/ssl/certs/ca-bundle.crt";

            # The nix-built tmux/ncurses searches only its own terminfo, ~/.terminfo and
            # TERMINFO_DIRS, so ssh sessions from kitty/ghostty need their entries here.
            home.file.".terminfo/x/xterm-kitty".source =
              "${pkgs.kitty.terminfo}/share/terminfo/x/xterm-kitty";
            home.file.".terminfo/x/xterm-ghostty".source =
              "${pkgs.ghostty.terminfo}/share/terminfo/x/xterm-ghostty";

            # zsh here always starts under a login bash that already ran the host's
            # /etc/profile; the host's global zsh files would source its profile.d
            # scripts again in every shell (most of a new tmux pane's startup time).
            programs.zsh.envExtra = "setopt no_global_rcs";

            # A namespace wrapper prevents zsh from setting USERNAME via getpwuid;
            # %n expands to USERNAME which stays empty. Replace with $USER after grml builds PROMPT.
            programs.zsh.initContent = ''
              add-zsh-hook precmd _fix_username_prompt
              _fix_username_prompt() {
                PROMPT="''${PROMPT//\%n/$USER}"
                RPROMPT="''${RPROMPT//\%n/$USER}"
              }

              export FZF_ALT_C_COMMAND="${pkgs.fd}/bin/fd --type d --hidden --no-ignore-vcs --exclude=.git --exclude=.jj --exclude=node_modules --exclude='bazel-*' --exclude=__pycache__ --exclude=.claude/worktrees --exclude=00_BLOG_00 --exclude=scratch"
              export FZF_CTRL_T_COMMAND="${pkgs.fd}/bin/fd --type f --type d --hidden --no-ignore-vcs --exclude=.git --exclude=.jj --exclude=node_modules --exclude='bazel-*' --exclude=__pycache__ --exclude=.claude/worktrees --exclude=00_BLOG_00 --exclude=scratch"
            '';
          }
        ];
      });
}

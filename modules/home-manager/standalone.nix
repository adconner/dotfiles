{ inputs, config, withSystem, ... }:
{
  flake.homeConfigurations.aconner =
    withSystem "x86_64-linux" ({ pkgs, ... }:
      inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          config.flake.modules.homeManager.base
          {
            home.username = "aconner";
            home.homeDirectory = "/home/aconner";

            home.sessionVariables.NIX_SSL_CERT_FILE = "/etc/ssl/certs/ca-bundle.crt";

            # nsw namespace prevents zsh from setting USERNAME via getpwuid;
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

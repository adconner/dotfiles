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
            # nsw namespace prevents zsh from setting USERNAME via getpwuid;
            # %n expands to USERNAME which stays empty. Replace with $USER after grml builds PROMPT.
            programs.zsh.initContent = ''
              add-zsh-hook precmd _fix_username_prompt
              _fix_username_prompt() {
                PROMPT="''${PROMPT//\%n/$USER}"
                RPROMPT="''${RPROMPT//\%n/$USER}"
              }
            '';
          }
        ];
      });
}

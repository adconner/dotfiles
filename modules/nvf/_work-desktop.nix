{ lib, pkgs, ... }:
{
  # The work desktop's nvim is for notes, and its store is built from source
  # (no cache.nixos.org): keep the markdown tooling, drop the coding language
  # servers, formatters and linters along with their toolchains.
  config.vim = {
    # nvf builds blink-cmp from its own nixpkgs instance, which lacks the work
    # desktop overlay (test fixes) and would duplicate python/rustc/LLVM builds.
    # nixpkgs names it blink.cmp, but nvf lazy-loads it by its pack directory
    # name, blink-cmp.
    pluginOverrides.blink-cmp = pkgs.vimPlugins.blink-cmp.overrideAttrs { pname = "blink-cmp"; };
    lsp.servers.texlab = {
      enable = lib.mkForce false;
      # The disabled server's cmd is still written to init.lua; don't let its
      # store path pull texlab into the build.
      cmd = lib.mkForce [ "texlab" ];
    };
    languages = {
      enableFormat = lib.mkForce false;
      enableExtraDiagnostics = lib.mkForce false;
      python.lsp.enable = false;
      clang.lsp.enable = false;
      rust.lsp.enable = false;
      nix.lsp.enable = false;
      typescript.lsp.enable = false;
      markdown = {
        # The defaults pull in .NET (marksman) and V8 (deno fmt).
        lsp.servers = [ "markdown-oxide" ];
        format = {
          enable = true;
          type = [ "prettier" ];
        };
        extraDiagnostics.enable = true;
      };
    };
  };
}

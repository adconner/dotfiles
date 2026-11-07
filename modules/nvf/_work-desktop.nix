{ inputs, lib, pkgs, ... }:
{
  # The work desktop's nvim is for notes, and its store is built from source
  # (no cache.nixos.org): keep the markdown tooling, drop the coding language
  # servers, formatters and linters along with their toolchains.
  config.vim = {
    # nvf builds blink-cmp from its own nixpkgs instance, which lacks the work
    # desktop overlay (test fixes) and would duplicate python/rustc/LLVM builds.
    # Build nvf's recipe with these pkgs instead (inputs here are nvf's).
    pluginOverrides.blink-cmp = pkgs.callPackage "${inputs.self}/flake/pkgs/by-name/blink-cmp/package.nix" { };
    languages = {
      enableFormat = lib.mkForce false;
      enableExtraDiagnostics = lib.mkForce false;
      python.lsp.enable = false;
      clang.lsp.enable = false;
      rust.lsp.enable = false;
      nix.lsp.enable = false;
      typescript.lsp.enable = false;
      tex.lsp.enable = false;
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

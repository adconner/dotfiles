{
  description = "System and environment configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nvf.url = "github:notashelf/nvf";
    nvf.inputs.nixpkgs.follows = "nixpkgs";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    import-tree.url = "github:vic/import-tree";
    resolver-nvim = {
      url = "github:yeshwanthyk/resolver.nvim";
      flake = false;
    };
    zsh-jj = {
      url = "github:rkh/zsh-jj";
      flake = false;
    };
    llama-cpp = {
      url = "github:ggml-org/llama.cpp";
      flake = false;
    };
  };

  outputs = { flake-parts, ... }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
    imports = [
      (inputs.import-tree ./modules)
    ];
    systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
    flake = {
      meta.owner = {
        name = "Austin Conner";
        username = "austin";
        email = "aconner.vu@gmail.com";
      };
    };
  };
}

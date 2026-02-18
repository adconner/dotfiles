{pkgs, inputs, ...}: 
{
  config.vim = {

    theme = {
      enable = true;
      name = "everforest";
      style = "hard";
      transparent = false;
    };

    extraPlugins = with pkgs.vimPlugins; {
      # wombat = {
      #   package = pkgs.vimUtils.buildVimPlugin {
      #     name = "wombat-nvim";
      #     src = inputs.wombat;
      #     nvimSkipModule = [
      #       "wombat.ansi_256mod"
      #       "wombat.ansi_ghostty"
      #       "wombat.ansi_iterm2"
      #       "wombat.ansi_lush"
      #     ];
      #   };
      #   # setup = "require('wombat').setup({})";
      #   after = [ "lush" ];
      # };
      # lush.package = lush-nvim;
      rsi.package = vim-rsi;
      bqf.package = nvim-bqf;
    };

    lsp = {
      enable = true;
      lspkind.enable = true;
      nvim-docs-view.enable = true;
      # harper-ls.enable = true;
    };

    startPlugins = with pkgs.vimPlugins; [ "vim-repeat" ];
    fzf-lua.enable = true;
    tabline.nvimBufferline.enable = true;
    visuals.nvim-web-devicons.enable = true;
    # visuals.indent-blankline.enable = true;
    utility.oil-nvim = {
      enable = true;
      gitStatus.enable = true;
    };
    git = {
      enable = true;
      gitsigns.mappings = {
        stageHunk = "<leader>ga";
        resetHunk = "<leader>gu";
        stageBuffer = "<leader>gw";
        resetBuffer = "<leader>gr";
        previewHunk = "<leader>gp";
        diffThis = "<leader>gd";
        toggleBlame = "<leader>gtb";
        toggleDeleted = "<leader>gtd";
      };
    };
    utility.sleuth.enable = true;
    comments.comment-nvim.enable = true;
    # autopairs.nvim-autopairs.enable = true;
    autocomplete.blink-cmp.enable = true;
    undoFile.enable = true;
    ui.fastaction.enable = true;
    clipboard.providers.wl-copy.enable = true;
    # debugger.nvim-dap = {
    #   enable = true;
    #   ui.enable = true;
    # };
    
    # lineNumberMode = "none";
    # utility.motion.leap = {
    #   enable = true;
    #   mappings.leapForwardTo = "f";
    #   mappings.leapBackwardTo = "F";
    # };

    # binds.whichKey.enable = true;
    # utility.nvim-biscuits.enable = true;
    # snippets.luasnip.enable = true;
    # treesitter.context.enable = true;
    # minimap.codewindow.enable = true;

    # yanky with hydra
    # smart-splits with tmux


    keymaps = [
      { action = "<Esc>"; key = "jj"; mode = "i"; }
      { action = ":FzfLua global<cr>"; key = "<c-p>"; mode = "n"; }
      { action = ":FzfLua live_grep<cr>"; key = "<c-g>"; mode = "n"; }
      { action = ":FzfLua buffers<cr>"; key = "<leader>b"; mode = "n"; }
      { action = ":Oil<cr>"; key = "-"; mode = "n"; }
      { action = "`[v`]"; key = "gp"; mode = "n"; }
      { action = ":resize<cr>:vertical resize<cr>"; key = "<leader>m"; mode = "n"; }
      { action = "<c-w>="; key = "<leader>M"; mode = "n"; }
      { action = ":%s/<c-r>//"; key = "<leader>r"; mode = "n"; }
      { action = ":s/\\%V<c-r>/\\%V/"; key = "<leader>r"; mode = "x"; }
      { action = ":Gitsigns toggle_word_diff<cr>"; key = "<leader>gtw"; mode = "n"; }
      { action = ":Gitsigns blame<cr>"; key = "<leader>gb"; mode = "n"; }
      { action = ":Git commit --allow-empty-message -m ''<cr>"; key = "<leader>gc"; mode = "n"; }
      { action = ":lua vim.lsp.stop_client(vim.lsp.get_clients())<cr>"; key = "<leader>cs"; mode = "n"; }
    ];

    languages = {
      enableFormat = true;
      enableTreesitter = true;
      enableExtraDiagnostics = true;

      python.enable = true;
      clang.enable = true;
      rust = {
        enable = true;
        extensions.crates-nvim.enable = true;
      };
      nix.enable = true;
      markdown.enable = true;
    };

    luaConfigRC.myconfig = /* lua */ ''
      vim.cmd [[
        command -bang Q q<bang>
        command -bang W w<bang>
        command -nargs=* -complete=file -bang Make make<bang> <args>
      ]]
      '';
  };
}

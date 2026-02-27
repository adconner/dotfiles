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
      nightfox.package = nightfox-nvim;
      wombat.package = wombat256-vim;
      rsi.package = vim-rsi;
      bqf.package = nvim-bqf;
      yank-assassin.package = YankAssassin-vim;
      vimtex.package = vimtex;
    };

    lsp = {
      enable = true;
      lspkind.enable = true;
      nvim-docs-view.enable = true;
      # harper-ls.enable = true     servers = {
      servers = {
        texlab = {
          enable = true;
          cmd = ["${pkgs.texlab}/bin/texlab"];
          filetypes = ["tex"];
        };
      };
    };

    startPlugins = with pkgs.vimPlugins; [ "vim-repeat" ];

    globals = {
      mapleader = "-";
      maplocalleader = "-";

      tex_flavor = "latex";
      vimtex_compiler_method = "latexmk";
      vimtex_view_method = "zathura";
      vimtex_compiler_latexmk = {
        callback = 1;
        continuous = 1;
        executable = "latexmk";
        hooks = [];
        options = [
          "-verbose"
          "-file-line-error"
          "-synctex=1"
          "-interaction=nonstopmode"
          "-shell-escape"
        ];
      };
    };

    fzf-lua.enable = true;
    tabline.nvimBufferline = {
      enable = true;
      mappings = {
        cycleNext = "]b";
        cyclePrevious = "[b";
        pick = null;
        sortByExtension = null;
        sortByDirectory = null;
        sortById = null;
        moveNext = null;
        movePrevious = null;
      };
      setupOpts.options = {
        always_show_bufferline = false;
        sort_by = "id";
      };
    };
    visuals.nvim-web-devicons.enable = true;
    # visuals.indent-blankline.enable = true;
    utility.oil-nvim = {
      enable = true;
      gitStatus.enable = true;
      setupOpts.keymaps = {
        "-" = false;
        " " = "actions.parent";
      };
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
    autocomplete.blink-cmp = {
      enable = true;
      setupOpts = {
        completion.list.selection.preselect = false;
      };
      friendly-snippets.enable = true;
    };
    undoFile.enable = true;
    # ui.fastaction.enable = true;
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
      { action = ":Oil<cr>"; key = " "; mode = "n"; }
      { action = "`[v`]"; key = "gp"; mode = "n"; }
      { action = ":resize<cr>:vertical resize<cr>"; key = "<leader>m"; mode = "n"; }
      { action = "<c-w>="; key = "<leader>M"; mode = "n"; }
      { action = ":%s/<c-r>//"; key = "<leader>r"; mode = "n"; }
      { action = ":s/\\%V<c-r>/\\%V/"; key = "<leader>r"; mode = "x"; }
      { action = "<Cmd>nohlsearch|diffupdate|normal! <c-l><CR>"; key = "<leader><space>"; mode = "n"; }
      { action = ":Gitsigns toggle_word_diff<cr>"; key = "<leader>gtw"; mode = "n"; }
      { action = ":Gitsigns blame<cr>"; key = "<leader>gb"; mode = "n"; }
      { action = ":Git commit --allow-empty-message -m ''<cr>"; key = "<leader>gc"; mode = "n"; }
      { action = ":lua vim.lsp.stop_client(vim.lsp.get_clients())<cr>"; key = "<leader>cs"; mode = "n"; }
    ];

    options = {
      shiftwidth = 2;
    };

    languages = {
      enableFormat = true;
      enableTreesitter = true;
      enableExtraDiagnostics = true;

      python = {
        enable = true;
        lsp.servers = [ "ruff" ];
        format.type = [ "ruff" ];
      };
      clang.enable = true;
      rust = {
        enable = true;
        extensions.crates-nvim.enable = true;
      };
      nix.enable = true;
      markdown.enable = true;
      ts.enable = true;
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

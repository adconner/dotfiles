{pkgs, lib, ...}:
{
  config.vim = {

    theme = {
      enable = true;
      name = "everforest";
      style = "hard";
      transparent = false;
    };

    extraPlugins = with pkgs.vimPlugins; {
      rsi.package = vim-rsi;
      bqf.package = nvim-bqf;
      yank-assassin.package = YankAssassin-vim;
      vimtex.package = vimtex; # maybe lazy load instead
      minuet-ai = {
        package = minuet-ai-nvim;
        setup = ''require('minuet').setup({
          provider = "openai_fim_compatible",
          provider_options = {
            openai_fim_compatible = {
              end_point = "http://localhost:8080/v1/completions",
              api_key = "TERM",
              name = "llama-cpp",
              stream = true,
              template = {
                prompt = function(context_before_cursor, context_after_cursor, _)
                    return '<|fim_prefix|>'
                        .. context_before_cursor
                        .. '<|fim_suffix|>'
                        .. context_after_cursor
                        .. '<|fim_middle|>'
                end,
                suffix = false,
              },
            },
          },
          duet = {
            provider = "openai_compatible",
            provider_options = {
              openai_compatible = {
                end_point = "http://localhost:8080/v1/chat/completions",
                api_key = "TERM",
                name = "llama-cpp",
              },
            },
          },
          virtualtext = {
            auto_trigger_ft = {},
            keymap = {
                -- accept whole completion
                accept = '<A-A>',
                -- accept one line
                accept_line = '<A-a>',
                -- accept n lines (prompts for number)
                -- e.g. "A-z 2 CR" will accept 2 lines
                accept_n_lines = '<A-z>',
                -- Cycle to prev completion item, or manually invoke completion
                prev = '<A-[>',
                -- Cycle to next completion item, or manually invoke completion
                next = '<A-]>',
                dismiss = '<A-e>',
            },
          },
        })'';
      };
    };
    lsp = {
      enable = true;
      lspkind.enable = true;
      nvim-docs-view.enable = true;
      # harper-ls.enable = true;
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

    fzf-lua = {
      enable = true;
      setupOpts = {
        keymap.fzf."ctrl-q" = "select-all+accept";
        fzf_opts = {
          "--exact" = true;
        };
      };
    };
    visuals.nvim-web-devicons.enable = true;
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
    # Inline images in markdown buffers via the kitty graphics protocol
    # (kitty, ghostty). snacks wraps the escapes for tmux passthrough itself.
    utility.snacks-nvim = {
      enable = true;
      setupOpts.image.enabled = true;
    };
    # snacks.image converts every format except PNG with ImageMagick.
    extraPackages = [ pkgs.imagemagick ];
    autocomplete.blink-cmp = {
      enable = true;
      setupOpts = {
        completion.list.selection.preselect = false;
        # tab seems to doesnt insert first match with tab, this seems to be a bug
        cmdline.enabled = false;
      };
      friendly-snippets.enable = true;
    };
    undoFile.enable = true;
    # ui.fastaction.enable = true;
    # debugger.nvim-dap = {
    #   enable = true;
    #   ui.enable = true;
    # };
    assistant.codecompanion-nvim = {
      enable = true;
      setupOpts = lib.generators.mkLuaInline ''{
        adapters = {
          http = {
            llamacpp = function()
              return require("codecompanion.adapters").extend("openai_compatible", {
                env = {
                  url = "http://127.0.0.1:8080",
                },
              })
            end,
          },
        },
        interactions = {
          chat = { adapter = "llamacpp", },
          inline = { adapter = "llamacpp", },
        }
      }'';
    };

    # yanky with hydra
    # smart-splits with tmux

    keymaps = [
      { action = "<Esc>"; key = "jj"; mode = "i"; }
      { action = "<C-\\><C-n>"; key = "jj"; mode = "t"; }
      { action = ":FzfLua global<cr>"; key = "<c-p>"; mode = "n"; }
      { action = ":FzfLua live_grep<cr>"; key = "<c-g>"; mode = "n"; }
      { action = ":FzfLua buffers<cr>"; key = "<leader>b"; mode = "n"; }
      { action = ":Oil<cr>"; key = " "; mode = "n"; }
      { action = "`[v`]"; key = "gp"; mode = "n"; }
      { action = ":resize<cr>:vertical resize<cr>"; key = "<leader>m"; mode = "n"; }
      { action = "<c-w>="; key = "<leader>M"; mode = "n"; }
      { action = ":%s/<c-r>//"; key = "<leader>r"; mode = "n"; }
      { action = ":s/\\%V<c-r>/\\%V/"; key = "<leader>r"; mode = "x"; }
      { action = "<Cmd>nohlsearch|diffupdate<CR>"; key = "<leader><space>"; mode = "n"; }
      { action = ":Gitsigns toggle_word_diff<cr>"; key = "<leader>gtw"; mode = "n"; }
      { action = ":Gitsigns blame<cr>"; key = "<leader>gb"; mode = "n"; }
      { action = ":Git commit --allow-empty-message -m ''<cr>"; key = "<leader>gc"; mode = "n"; }
      { action = ":lua vim.lsp.stop_client(vim.lsp.get_clients())<cr>"; key = "<leader>cs"; mode = "n"; }
    ];

    clipboard = {
      enable = true;
      registers = "unnamedplus";
      providers.wl-copy = {
        enable = true;
        package = null;
      };
    };

    options = {
      shiftwidth = 2;
    };

    languages = {
      enableFormat = true;
      enableTreesitter = true;
      enableExtraDiagnostics = true;

      python = {
        enable = true;
        # lsp.servers = [ "ruff" ];
        format.type = [ "ruff" ];
      };
      clang.enable = true;
      rust = {
        enable = true;
        extensions.crates-nvim.enable = true;
      };
      nix.enable = true;
      markdown = {
        enable = true;
        extensions.render-markdown-nvim.enable = true;
      };
      typescript.enable = true;
    };

    luaConfigRC.osc52 = /* lua */ ''
      -- Headless remote session: no local X11/Wayland clipboard exists, so
      -- force nvim's built-in OSC 52 provider (copy escapes forward through
      -- tmux/mosh to the terminal; paste queries are answered by tmux itself
      -- via its get-clipboard=buffer default).
      local function present(v)
        return v ~= nil and #v > 0
      end
      if not present(vim.env.DISPLAY) and not present(vim.env.WAYLAND_DISPLAY) then
        vim.g.clipboard = 'osc52'
      end
      '';

    luaConfigRC.myconfig = /* lua */ ''
      vim.cmd [[
        command -bang Q q<bang>
        command -bang W w<bang>
        command -nargs=* -complete=file -bang Make make<bang> <args>
      ]]
      '';


  };
}

local utils = require("config.utils")

return {
  {
    "nvim-telescope/telescope.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope-project.nvim",
      "debugloop/telescope-undo.nvim",
      "piersolenski/telescope-import.nvim",
    },
    cmd = "Telescope",
    keys = {
      { "<leader>,", vim.NIL },
      { "<leader>:", vim.NIL },
      { "<leader>/", vim.NIL },
      { "<leader><space>", vim.NIL },
      { "<leader>sd", vim.NIL },
      { "<leader>sD", vim.NIL },
      { "<leader>sg", vim.NIL },
      { "<leader>sG", vim.NIL },
      { "<leader>ss", vim.NIL },
      { "<leader>sS", vim.NIL },
      { "<leader>sb", vim.NIL },
      { "<leader>sw", vim.NIL },
      { "<leader>sW", vim.NIL },
      { "<leader>sw", vim.NIL, mode = "v" },
      { "<leader>sW", vim.NIL, mode = "v" },
      { "<leader>fw", LazyVim.pick("grep_string", { root = false, word_match = "-w" }), desc = "Find Word" },
      { "<leader>fw", LazyVim.pick("grep_string", { root = false }), mode = "v", desc = "Find Selection" },
      { "<leader>fb", "<cmd>Telescope current_buffer_fuzzy_find<cr><cr>", desc = "Find in Buffer" },
      { "<leader>fg", LazyVim.pick("live_grep"), desc = "Grep" },
      { "<leader>p", "<cmd>lua require('telescope').extensions.project.project({})<cr>", desc = "Projects" },
      { "<leader>U", "<cmd>Telescope undo<cr>", desc = "undo tree" },
      { "<leader>ci", "<cmd>Telescope import<cr>", desc = "Import" },
    },
    config = function()
      require("telescope").setup({
        defaults = {
          sorting_strategy = "ascending",
          scroll_strategy = "limit",
          wrap_results = false,
          dynamic_preview_title = true,
          prompt_prefix = "  ",
          path_display = { "smart" },
          borderchars = utils.border_chars_outer_thin_telescope,
          border = true,
          file_ignore_patterns = {
            "node_modules/",
            ".terraform/",
            ".git/",
            "*.webp",
            "$RECYCLE.BIN",
          },
          preview = {
            filesize_limit = 2,
            timeout = 200,
          },
          mappings = {
            i = {
              ["<C-j>"] = require("telescope.actions").move_selection_next,
              ["<C-k>"] = require("telescope.actions").move_selection_previous,
              ["<C-H>"] = function()
                vim.api.nvim_input("<C-w>")
              end,
              ["<c-q>"] = require("trouble.sources.telescope").open,
            },
            n = {
              ["<c-q>"] = require("trouble.sources.telescope").open,
            },
          },
        },
        pickers = {
          find_files = {
            hidden = true,
          },
          buffers = {
            theme = "dropdown",
            previewer = true,
            path_display = { shorten = { 1, exclude = { -2, -1 } } },
            borderchars = utils.border_chars_outer_thin_telescope,
          },
        },
        extensions = {
          fzf = {
            fuzzy = true, -- false will only do exact matching
            override_generic_sorter = true, -- override the generic sorter
            override_file_sorter = true, -- override the file sorter
            case_mode = "smart_case", -- or "ignore_case" or "respect_case"
          },
          project = {
            base_dirs = {
              "~",
              "D:\\",
            },
            hidden_files = true,
            theme = "dropdown",
            order_by = "recent",
            search_by = "title",
            on_project_selected = function(prompt_bufnr)
              local selected = require("telescope._extensions.project.actions").get_selected_path()
              local stat = vim.loop.fs_stat(selected .. "/.bare")
              if stat and stat.type == "directory" then
                -- contains git worktrees (sibling .bare directory)
                utils.switch_git_worktree(selected)
              else
                -- regular directory
                require("telescope._extensions.project.actions").find_project_files(prompt_bufnr, false)
                vim.cmd("tabnew")
              end
            end,
          },
          undo = {
            use_delta = false,
          },
          import = {
            insert_at_top = false,
          },
        },
      })
      require("telescope").load_extension("undo")
      require("telescope").load_extension("import")
    end,
  },
}

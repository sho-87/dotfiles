local git = require("utils.git")

vim.keymap.set("n", "<leader>gw", git.switch_git_worktree, { desc = "Worktrees" })

return {
  {
    "pwntester/octo.nvim",
    event = "LazyFile",
    cmd = "Octo",
    ft = "octo",
    keys = {
      { "<leader>a", false, ft = "octo" },
      { "<leader>c", false, ft = "octo" },
      { "<leader>l", false, ft = "octo" },
      { "<leader>i", false, ft = "octo" },
      { "<leader>r", false, ft = "octo" },
      { "<leader>p", false, ft = "octo" },
      { "<leader>v", false, ft = "octo" },
      { "<localleader>a", "", desc = "assignee (Octo)", ft = "octo" },
      { "<localleader>c", "", desc = "comment/code (Octo)", ft = "octo" },
      { "<localleader>l", "", desc = "label (Octo)", ft = "octo" },
      { "<localleader>i", "", desc = "issue (Octo)", ft = "octo" },
      { "<localleader>r", "", desc = "react (Octo)", ft = "octo" },
      { "<localleader>p", "", desc = "pr (Octo)", ft = "octo" },
      { "<localleader>v", "", desc = "review (Octo)", ft = "octo" },
    },
    opts = {
      ssh_aliases = { ["personal-github.com"] = "github.com" },
      use_local_fs = true,
      enable_builtin = false,
      default_to_projects_v2 = true,
      mappings_disable_default = true,
      mappings = {
        issue = {
          close_issue = { lhs = "<localleader>ic", desc = "close issue" },
          reopen_issue = { lhs = "<localleader>io", desc = "reopen issue" },
          list_issues = { lhs = "<localleader>il", desc = "list open issues on same repo" },
          reload = { lhs = "<C-r>", desc = "reload issue" },
          open_in_browser = { lhs = "<C-b>", desc = "open issue in browser" },
          copy_url = { lhs = "<C-y>", desc = "copy url to clipboard" },
          add_assignee = { lhs = "<localleader>aa", desc = "add assignee" },
          remove_assignee = { lhs = "<localleader>ad", desc = "remove assignee" },
          create_label = { lhs = "<localleader>lc", desc = "create label" },
          add_label = { lhs = "<localleader>la", desc = "add label" },
          remove_label = { lhs = "<localleader>ld", desc = "remove label" },
          goto_issue = { lhs = "gi", desc = "navigate to a repo issue" },
          add_comment = { lhs = "<localleader>ca", desc = "add comment" },
          delete_comment = { lhs = "<localleader>cd", desc = "delete comment" },
          next_comment = { lhs = "]x", desc = "Next comment" },
          prev_comment = { lhs = "[x", desc = "Previous comment" },
          react_hooray = { lhs = "<localleader>rp", desc = "toggle 🎉" },
          react_heart = { lhs = "<localleader>rh", desc = "toggle ❤️" },
          react_eyes = { lhs = "<localleader>re", desc = "toggle 👀" },
          react_thumbs_up = { lhs = "<localleader>r+", desc = "toggle 👍" },
          react_thumbs_down = { lhs = "<localleader>r-", desc = "toggle 👎" },
          react_rocket = { lhs = "<localleader>rr", desc = "toggle 🚀" },
          react_laugh = { lhs = "<localleader>rl", desc = "toggle 😄" },
          react_confused = { lhs = "<localleader>rc", desc = "toggle 😕" },
        },
        pull_request = {
          checkout_pr = { lhs = "<localleader>po", desc = "checkout PR" },
          merge_pr = { lhs = "<localleader>pm", desc = "merge commit PR" },
          squash_and_merge_pr = { lhs = "<localleader>psm", desc = "squash and merge PR" },
          rebase_and_merge_pr = { lhs = "<localleader>prm", desc = "rebase and merge PR" },
          list_commits = { lhs = "<localleader>pc", desc = "list PR commits" },
          list_changed_files = { lhs = "<localleader>pf", desc = "list PR changed files" },
          show_pr_diff = { lhs = "<localleader>pd", desc = "show PR diff" },
          add_reviewer = { lhs = "<localleader>va", desc = "add reviewer" },
          remove_reviewer = { lhs = "<localleader>vd", desc = "remove reviewer request" },
          close_issue = { lhs = "<localleader>ic", desc = "close PR" },
          reopen_issue = { lhs = "<localleader>io", desc = "reopen PR" },
          list_issues = { lhs = "<localleader>il", desc = "list open issues on same repo" },
          reload = { lhs = "<C-r>", desc = "reload PR" },
          open_in_browser = { lhs = "<C-b>", desc = "open PR in browser" },
          copy_url = { lhs = "<C-y>", desc = "copy url to clipboard" },
          goto_file = { lhs = "gf", desc = "go to file" },
          add_assignee = { lhs = "<localleader>aa", desc = "add assignee" },
          remove_assignee = { lhs = "<localleader>ad", desc = "remove assignee" },
          create_label = { lhs = "<localleader>lc", desc = "create label" },
          add_label = { lhs = "<localleader>la", desc = "add label" },
          remove_label = { lhs = "<localleader>ld", desc = "remove label" },
          goto_issue = { lhs = "gi", desc = "navigate to a repo issue" },
          add_comment = { lhs = "<localleader>ca", desc = "add comment" },
          delete_comment = { lhs = "<localleader>cd", desc = "delete comment" },
          next_comment = { lhs = "]x", desc = "Next comment" },
          prev_comment = { lhs = "[x", desc = "Previous comment" },
          react_hooray = { lhs = "<localleader>rp", desc = "toggle 🎉" },
          react_heart = { lhs = "<localleader>rh", desc = "toggle ❤️" },
          react_eyes = { lhs = "<localleader>re", desc = "toggle 👀" },
          react_thumbs_up = { lhs = "<localleader>r+", desc = "toggle 👍" },
          react_thumbs_down = { lhs = "<localleader>r-", desc = "toggle 👎" },
          react_rocket = { lhs = "<localleader>rr", desc = "toggle 🚀" },
          react_laugh = { lhs = "<localleader>rl", desc = "toggle 😄" },
          react_confused = { lhs = "<localleader>rc", desc = "toggle 😕" },
          review_start = { lhs = "<localleader>vs", desc = "start review for PR" },
          review_resume = { lhs = "<localleader>vr", desc = "resume pending review for PR" },
        },
        review_thread = {
          goto_issue = { lhs = "gi", desc = "navigate to a local repo issue" },
          add_comment = { lhs = "<localleader>ca", desc = "add comment" },
          add_suggestion = { lhs = "<localleader>sa", desc = "add suggestion" },
          delete_comment = { lhs = "<localleader>cd", desc = "delete comment" },
          next_comment = { lhs = "]x", desc = "Next comment" },
          prev_comment = { lhs = "[x", desc = "Previous comment" },
          select_next_entry = { lhs = "]q", desc = "Next changed file" },
          select_prev_entry = { lhs = "[q", desc = "Previous changed file" },
          select_first_entry = { lhs = "[Q", desc = "First changed file" },
          select_last_entry = { lhs = "]Q", desc = "Previous changed file" },
          close_review_tab = { lhs = "<C-c>", desc = "close review tab" },
          react_hooray = { lhs = "<localleader>rp", desc = "toggle 🎉" },
          react_heart = { lhs = "<localleader>rh", desc = "toggle ❤️" },
          react_eyes = { lhs = "<localleader>re", desc = "toggle 👀" },
          react_thumbs_up = { lhs = "<localleader>r+", desc = "toggle 👍" },
          react_thumbs_down = { lhs = "<localleader>r-", desc = "toggle 👎" },
          react_rocket = { lhs = "<localleader>rr", desc = "toggle 🚀" },
          react_laugh = { lhs = "<localleader>rl", desc = "toggle 😄" },
          react_confused = { lhs = "<localleader>rc", desc = "toggle 😕" },
        },
        submit_win = {
          approve_review = { lhs = "<C-a>", desc = "approve review" },
          comment_review = { lhs = "<C-m>", desc = "comment review" },
          request_changes = { lhs = "<C-r>", desc = "request changes review" },
          close_review_tab = { lhs = "<C-c>", desc = "close review tab" },
        },
        review_diff = {
          submit_review = { lhs = "<localleader>vs", desc = "submit review" },
          discard_review = { lhs = "<localleader>vd", desc = "discard review" },
          add_review_comment = { lhs = "<localleader>ca", desc = "add new review comment" },
          add_review_suggestion = { lhs = "<localleader>sa", desc = "add new review suggestion" },
          focus_files = { lhs = "<localleader>e", desc = "focus changed file panel" },
          toggle_files = { lhs = "<localleader>b", desc = "hide/show changed files panel" },
          next_thread = { lhs = "]t", desc = "Next thread" },
          prev_thread = { lhs = "[t", desc = "Previous thread" },
          select_next_entry = { lhs = "]q", desc = "Next changed file" },
          select_prev_entry = { lhs = "[q", desc = "Previous changed file" },
          select_first_entry = { lhs = "[Q", desc = "First changed file" },
          select_last_entry = { lhs = "]Q", desc = "Previous changed file" },
          close_review_tab = { lhs = "<C-c>", desc = "close review tab" },
          toggle_viewed = { lhs = "<localleader><localleader>", desc = "toggle viewed state" },
          goto_file = { lhs = "gf", desc = "go to file" },
        },
        file_panel = {
          submit_review = { lhs = "<localleader>vs", desc = "submit review" },
          discard_review = { lhs = "<localleader>vd", desc = "discard review" },
          next_entry = { lhs = "j", desc = "Next changed file" },
          prev_entry = { lhs = "k", desc = "Previous changed file" },
          select_entry = { lhs = "<cr>", desc = "show selected changed file diffs" },
          refresh_files = { lhs = "R", desc = "refresh changed files panel" },
          focus_files = { lhs = "<localleader>e", desc = "focus changed file panel" },
          toggle_files = { lhs = "<localleader>b", desc = "hide/show changed files panel" },
          select_next_entry = { lhs = "]q", desc = "Next changed file" },
          select_prev_entry = { lhs = "[q", desc = "Previous changed file" },
          select_first_entry = { lhs = "[Q", desc = "First changed file" },
          select_last_entry = { lhs = "]Q", desc = "Previous changed file" },
          close_review_tab = { lhs = "<C-c>", desc = "close review tab" },
          toggle_viewed = { lhs = "<localleader><localleader>", desc = "toggle viewed state" },
        },
      },
    },
  },
  {
    "sindrets/diffview.nvim",
    event = "LazyFile",
    cmd = { "DiffviewOpen", "DiffviewFileHistory" },
    init = function(_)
      local wk = require("which-key")
      wk.add({
        { "<leader>gd", name = "Diffview", icon = "" },
      })
    end,
    keys = {
      { "<leader>gd", "<cmd>DiffviewOpen<cr>", desc = "Diffview" },
      { "<leader>gf", "<cmd>DiffviewFileHistory %<cr>", desc = "File History" },
    },
    opts = function()
      local actions = require("diffview.config").actions
      return {
        enhanced_diff_hl = true,
        view = {
          default = {
            -- Config for changed files, and staged files in diff views.
            layout = "diff2_horizontal",
            winbar_info = true,
          },
          merge_tool = {
            -- Config for conflicted files in diff views during a merge or rebase.
            layout = "diff3_mixed",
            disable_diagnostics = true,
            winbar_info = true,
          },
          file_history = {
            -- Config for changed files in file history views.
            layout = "diff2_horizontal",
            winbar_info = true,
          },
        },
        hooks = {
          diff_buf_win_enter = function(bufnr, winid, ctx)
            -- Turn off cursor line for diffview windows because of bg conflict
            -- https://github.com/neovim/neovim/issues/9800
            vim.wo[winid].culopt = "number"
          end,
        },
        keymaps = {
          disable_defaults = true,
          view = {
            {
              "n",
              "<tab>",
              actions.select_next_entry,
              { desc = "Open the diff for the next file" },
            },
            {
              "n",
              "<s-tab>",
              actions.select_prev_entry,
              { desc = "Open the diff for the previous file" },
            },
            {
              "n",
              "gf",
              actions.goto_file_edit,
              { desc = "Open the file in the previous tabpage" },
            },
            {
              "n",
              "<leader>e",
              actions.toggle_files,
              { desc = "Toggle the file panel." },
            },
            {
              "n",
              "[x",
              actions.prev_conflict,
              { desc = "In the merge-tool: jump to the previous conflict" },
            },
            {
              "n",
              "]x",
              actions.next_conflict,
              { desc = "In the merge-tool: jump to the next conflict" },
            },
            {
              "n",
              "<leader>gmo",
              actions.conflict_choose("ours"),
              { desc = "Choose the OURS version" },
            },
            {
              "n",
              "<leader>gmt",
              actions.conflict_choose("theirs"),
              { desc = "Choose the THEIRS version" },
            },
            {
              "n",
              "<leader>gmb",
              actions.conflict_choose("base"),
              { desc = "Choose the BASE version" },
            },
            {
              "n",
              "<leader>gma",
              actions.conflict_choose("all"),
              { desc = "Choose all the versions" },
            },
            {
              "n",
              "<leader>gmx",
              actions.conflict_choose("none"),
              { desc = "Delete the conflict region" },
            },
            {
              "n",
              "<leader>gmO",
              actions.conflict_choose_all("ours"),
              { desc = "Choose the OURS version for the whole file" },
            },
            {
              "n",
              "<leader>gmT",
              actions.conflict_choose_all("theirs"),
              { desc = "Choose the THEIRS version for the whole file" },
            },
            {
              "n",
              "<leader>gmB",
              actions.conflict_choose_all("base"),
              { desc = "Choose the BASE version for the whole file" },
            },
            {
              "n",
              "<leader>gmA",
              actions.conflict_choose_all("all"),
              { desc = "Choose all the versions for the whole file" },
            },
            {
              "n",
              "<leader>gmX",
              actions.conflict_choose_all("none"),
              { desc = "Delete the conflict region for the whole file" },
            },
          },
          diff1 = {
            {
              "n",
              "g?",
              actions.help({ "view", "diff1" }),
              { desc = "Open the help panel" },
            },
          },
          diff2 = {
            {
              "n",
              "g?",
              actions.help({ "view", "diff2" }),
              { desc = "Open the help panel" },
            },
          },
          diff3 = {
            {
              "n",
              "g?",
              actions.help({ "view", "diff3" }),
              { desc = "Open the help panel" },
            },
          },
          diff4 = {
            {
              "n",
              "g?",
              actions.help({ "view", "diff4" }),
              { desc = "Open the help panel" },
            },
          },
          file_panel = {
            {
              "n",
              "<cr>",
              actions.select_entry,
              { desc = "Open the diff for the selected entry" },
            },
            {
              "n",
              "<2-LeftMouse>",
              actions.select_entry,
              { desc = "Open the diff for the selected entry" },
            },
            { "n", "za", actions.toggle_fold, { desc = "Toggle fold" } },
            { "n", "zR", actions.open_all_folds, { desc = "Expand all folds" } },
            { "n", "zM", actions.close_all_folds, { desc = "Collapse all folds" } },
            {
              "n",
              "<tab>",
              actions.select_next_entry,
              { desc = "Open the diff for the next file" },
            },
            {
              "n",
              "<s-tab>",
              actions.select_prev_entry,
              { desc = "Open the diff for the previous file" },
            },
            {
              "n",
              "gf",
              actions.goto_file_edit,
              { desc = "Open the file in the previous tabpage" },
            },
            {
              "n",
              "R",
              actions.refresh_files,
              { desc = "Update stats and entries in the file list" },
            },
            {
              "n",
              "<leader>e",
              actions.toggle_files,
              { desc = "Toggle the file panel" },
            },
            {
              "n",
              "[x",
              actions.prev_conflict,
              { desc = "Go to the previous conflict" },
            },
            {
              "n",
              "]x",
              actions.next_conflict,
              { desc = "Go to the next conflict" },
            },
            {
              "n",
              "g?",
              actions.help("file_panel"),
              { desc = "Open the help panel" },
            },
            {
              "n",
              "<leader>gmO",
              actions.conflict_choose_all("ours"),
              { desc = "Choose the OURS version for the whole file" },
            },
            {
              "n",
              "<leader>gmT",
              actions.conflict_choose_all("theirs"),
              { desc = "Choose the THEIRS version for the whole file" },
            },
            {
              "n",
              "<leader>gmB",
              actions.conflict_choose_all("base"),
              { desc = "Choose the BASE version for the whole file" },
            },
            {
              "n",
              "<leader>gmA",
              actions.conflict_choose_all("all"),
              { desc = "Choose all the versions for the whole file" },
            },
            {
              "n",
              "<leader>gmX",
              actions.conflict_choose_all("none"),
              { desc = "Delete the conflict region for the whole file" },
            },
          },
          file_history_panel = {
            { "n", "g!", actions.options, { desc = "Open the option panel" } },
            {
              "n",
              "y",
              actions.copy_hash,
              { desc = "Copy the commit hash of the entry under the cursor" },
            },
            { "n", "za", actions.toggle_fold, { desc = "Toggle fold" } },
            { "n", "zR", actions.open_all_folds, { desc = "Expand all folds" } },
            { "n", "zM", actions.close_all_folds, { desc = "Collapse all folds" } },
            {
              "n",
              "<cr>",
              actions.select_entry,
              { desc = "Open the diff for the selected entry" },
            },
            {
              "n",
              "<2-LeftMouse>",
              actions.select_entry,
              { desc = "Open the diff for the selected entry" },
            },
            {
              "n",
              "<tab>",
              actions.select_next_entry,
              { desc = "Open the diff for the next file" },
            },
            {
              "n",
              "<s-tab>",
              actions.select_prev_entry,
              { desc = "Open the diff for the previous file" },
            },
            {
              "n",
              "gf",
              actions.goto_file_edit,
              { desc = "Open the file in the previous tabpage" },
            },
            {
              "n",
              "<leader>e",
              actions.toggle_files,
              { desc = "Toggle the file panel" },
            },
            {
              "n",
              "g?",
              actions.help("file_history_panel"),
              { desc = "Open the help panel" },
            },
          },
          option_panel = {
            {
              "n",
              "<space>",
              actions.select_entry,
              { desc = "Change the current option" },
            },
            { "n", "q", actions.close, { desc = "Close the panel" } },
            {
              "n",
              "g?",
              actions.help("option_panel"),
              { desc = "Open the help panel" },
            },
          },
          help_panel = {
            { "n", "q", actions.close, { desc = "Close help menu" } },
            { "n", "<esc>", actions.close, { desc = "Close help menu" } },
          },
        },
      }
    end,
  },
}

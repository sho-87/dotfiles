-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath "data" .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system {
      "git",
      "clone",
      "--filter=blob:none",
      "--single-branch",
      "https://github.com/folke/lazy.nvim.git",
      lazypath,
  }
end
vim.opt.runtimepath:prepend(lazypath)

-- Install plugin modules
require("lazy").setup({
    spec = {
        { import = "plugins.modules" }
    },
    defaults = {
        lazy = false,
        version = "*"
    },
    checker = { enabled = true, notify = false },
})

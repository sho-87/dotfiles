local M = {
  'VonHeikemen/lsp-zero.nvim',
  cond = vim.g.vscode == nil,
  enabled = true,
  branch = 'v1.x',
  dependencies = {
    -- LSP Support
    { 'neovim/nvim-lspconfig' }, -- Required
    { 'williamboman/mason.nvim' }, -- Optional
    { 'williamboman/mason-lspconfig.nvim' }, -- Optional

    -- Autocompletion
    { 'hrsh7th/nvim-cmp' }, -- Required
    { 'hrsh7th/cmp-nvim-lsp' }, -- Required
    { 'hrsh7th/cmp-buffer' }, -- Optional
    { 'hrsh7th/cmp-path' }, -- Optional
    { 'saadparwaiz1/cmp_luasnip' }, -- Optional
    { 'hrsh7th/cmp-nvim-lua' }, -- Optional

    -- Snippets
    { 'L3MON4D3/LuaSnip' }, -- Required
    { 'rafamadriz/friendly-snippets' }, -- Optional
  },
  event = 'VeryLazy',
}

function M.config()
  local lsp = require('lsp-zero').preset({
    name = 'recommended',
    set_lsp_keymaps = false,
    manage_nvim_cmp = true,
    suggest_lsp_servers = true,
  })

  lsp.on_attach(function(client, bufnr)
    local map = vim.keymap.set

    map('n', '<leader>g', '{}', { desc = "LSP" }) -- prefix
    map('n', '<leader>gD', '<cmd>lua vim.lsp.buf.declaration()<cr>', { desc = "Declaration", buffer = bufnr })
    map('n', '<leader>gd', '<cmd>lua vim.lsp.buf.definition()<cr>', { desc = "Definition", buffer = bufnr })
    map('n', '<leader>gt', '<cmd>lua vim.lsp.buf.type_definition()<cr>', { desc = "Type Definition", buffer = bufnr })
    map('n', '<leader>gr', '<cmd>lua vim.lsp.buf.references()<cr>', { desc = "Find all references", buffer = bufnr })
    map('n', '<leader>gR', '<cmd>lua vim.lsp.buf.rename()<cr>', { desc = "Rename", buffer = bufnr })
    map('n', '<leader>gi', '<cmd>lua vim.lsp.buf.implementation()<cr>', { desc = "Implementation", buffer = bufnr })
    map('n', '<leader>gf', '<cmd>lua vim.lsp.buf.format{async=true}<cr>', { desc = "Format", buffer = bufnr })
    map('n', '<leader>gs', '<cmd>lua vim.lsp.buf.signature_help()<cr>', { desc = "Signature", buffer = bufnr })
    map('n', '<leader>gh', '<cmd>lua vim.lsp.buf.hover()<cr>', { desc = "Hover", buffer = bufnr })
    map('n', '<leader>ga', '<cmd>lua vim.lsp.buf.code_action()<cr>', { desc = "Code Action", buffer = bufnr })
    map('n', '<leader>ge', '<cmd>lua vim.diagnostic.open_float()<cr>', { desc = "Show Error", buffer = bufnr })
    map('n', '<leader>g[', '<cmd>lua vim.diagnostic.goto_prev()<cr>', { desc = "Prev", buffer = bufnr })
    map('n', '<leader>g]', '<cmd>lua vim.diagnostic.goto_next()<cr>', { desc = "Next", buffer = bufnr })
  end)

  -- (Optional) Configure lua language server for neovim
  lsp.nvim_workspace()
  lsp.setup()

  vim.diagnostic.config({
    virtual_text = false,
  })
end

return M

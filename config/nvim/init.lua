vim.opt.relativenumber = true
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.wrap = false
vim.g.mapleader = ' '
vim.opt.clipboard = "unnamedplus"

vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })

vim.api.nvim_set_keymap('v', '<leader>y', '"+y', { noremap = true, silent = true })

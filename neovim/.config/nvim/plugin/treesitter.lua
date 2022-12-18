local ok, ts_configs = pcall(require, 'nvim-treesitter.configs')

if not ok then
	print("nvim-treesitter.configs doesn't exits")
	return
end

ts_configs.setup {
	ensure_installed = { "c", "lua", "python", "javascript"},
	sync_install = false, -- Install parsers synchronously (only applied to `ensure_installed`)
	auto_install = true, -- Automatically install missing parsers when entering buffer
	highlight = {
		enable = true,
		-- disable = {"c"},
		additional_vim_regex_highlighting = {'org'},
	},
	indent = {
		enable = {"javascript"},
	}
}

vim.keymap.set('n', '<leader>th', '<cmd>TSHighlightCapturesUnderCursor<cr>')
vim.keymap.set('n', '<leader>tn', '<cmd>TSNodeUnderCursor<cr>')

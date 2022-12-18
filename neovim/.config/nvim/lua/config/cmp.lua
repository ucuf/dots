local cmp = require('cmp')
local luasnip = require('luasnip')
local lspkind = require('lspkind')

vim.opt.completeopt={'menu', 'menuone', 'noselect'}
cmp.setup({
	snippet = {
		expand = function(args)
			luasnip.lsp_expand(args.body)
		end,
	},
	mapping = cmp.mapping.preset.insert({
		['<C-Space>'] = cmp.mapping.complete(),
		['<C-b>'] = cmp.mapping.scroll_docs(-4),
		['<C-f>'] = cmp.mapping.scroll_docs(4),
		['<C-e>'] = cmp.mapping.abort(),
		['<CR>'] = cmp.mapping.confirm({select = true}),
	}),
	sources = cmp.config.sources({
		{name = 'luasnip'},
		{name = 'nvim_lsp'},
		-- {name = 'nvim_lua'},
	}, {
		{name = 'buffer', keyword_length = 5},
	}),
	formatting = {
		format = lspkind.cmp_format({
			with_text = true,
			menu = ({
				luasnip = '[Snip]',
				nvim_lsp = '[LSP]',
				nvim_lua = '[Lua]',
				buffer = '[Buf]',
			})
		}),
	}
})

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
-- cmp.setup.cmdline(':', {
-- 	mapping = cmp.mapping.preset.cmdline(),
-- 	sources = cmp.config.sources({
-- 		{name = 'path'}
-- 	}, {
-- 		{name = 'cmdline'}
-- 	})
-- })

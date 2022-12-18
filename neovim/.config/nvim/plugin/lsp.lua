local lspconfig = require('lspconfig')
local lsp = vim.lsp;
local null_ls = require 'null-ls'

local opts = { noremap = true, silent = true }
local on_attach = function(client, bufnr)
	-- Enable completion triggered by <c-x><c-o>
	vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

	-- Mappings.
	-- See `:help vim.lsp.*` for documentation on any of the below functions
	local bufopts = { noremap = true, silent = true, buffer = bufnr }
	vim.keymap.set('n', 'gD', lsp.buf.declaration, bufopts)
	vim.keymap.set('n', 'gd', lsp.buf.definition, bufopts)
	vim.keymap.set('n', 'K', lsp.buf.hover, bufopts)
	vim.keymap.set('n', '<space>gi', lsp.buf.implementation, bufopts)
	vim.keymap.set('n', '<space>ca', lsp.buf.code_action, bufopts)
	vim.keymap.set('n', '<space>rn', lsp.buf.rename, bufopts)
	vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
	vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
	vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, opts)
	vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, opts)

	-- vim.keymap.set('n', '<C-k>', lsp.buf.signature_help, bufopts)
	vim.keymap.set('n', '<space>wa', lsp.buf.add_workspace_folder, bufopts)
	vim.keymap.set('n', '<space>wr', lsp.buf.remove_workspace_folder, bufopts)
	vim.keymap.set('n', '<space>wl', function()
		print(vim.inspect(lsp.buf.list_workspace_folders()))
	end, bufopts)
	vim.keymap.set('n', '<space>D', lsp.buf.type_definition, bufopts)
	vim.keymap.set('n', '<space>gr', lsp.buf.references, bufopts)

	if client.server_capabilities.documentFormattingProvider then
		vim.keymap.set('n', '<space>f', lsp.buf.format, bufopts)
	end
end

local servers = {
	tsserver      = {
		prefer_null_ls = true,
	},
	pyright       = {},
	eslint        = {},
	bashls        = {},
	cssmodules_ls = {},
	cssls         = {},
	ltex          = {},
	texlab        = {},
	sumneko_lua   = {
		settings = {
			Lua = {
				runtime = {
					-- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
					version = 'Lua 5.1',
				},
				completion = {
					displayContext = 3,
					showWord = "Disable",
				},
				hint = {
					enable = true,
					arrayIndex = "Enable",
					setType = true,
				},
				diagnostics = {
					-- Get the language server to recognize the `vim` global
					globals = {
						'vim',

						-- awesomewm
						'awesome',
						'client',
						'screen',
						'root',
					},
				},
				workspace = {
					-- Make the server aware of Neovim runtime files
					-- library = vim.api.nvim_get_runtime_file("", true),
					library = {
						['/usr/share/awesome/lib'] = true,
						[vim.fn.expand("$VIMRUNTIME/lua")] = true,
						[vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true,
					},
				},
				-- Do not send telemetry data containing a randomized but unique identifier
				telemetry = {
					enable = false,
				},
			},
		},
	}
}

local function prefer_null_ls_fmt(client, bufnr)
	-- client.server_capabilities.documentHighlightProvider = false
	client.resolved_capabilities.document_formatting = false
	-- TODO: use this instead in 0.8 and later
	client.resolved_capabilities.documentFormattingProvider = false
	client.resolved_capabilities.documentRangeFormattingProvider = false

	on_attach(client, bufnr)
end

local client_capabilities = lsp.protocol.make_client_capabilities()
local completionItem = client_capabilities.textDocument.completion.completionItem

completionItem.snippetSupport = true
completionItem.resolveSupport = {
	properties = { 'documentation', 'detail', 'additionalTextEdits' },
}

client_capabilities = require('cmp_nvim_lsp').default_capabilities(client_capabilities)
client_capabilities.offsetEncoding = { 'utf-16' }



-- require('clangd_extensions.config').setup {
require('clangd_extensions').setup {
	server = {
		-- options to pass to nvim-lspconfig
		-- i.e. the arguments to require("lspconfig").clangd.setup({})
		on_attach = on_attach,
		capabilities = client_capabilities,
	},
	extensions = {
		inlay_hints = {
			only_current_line = false,
			show_parameter_hints = true,
		},
	},
}

for server, config in pairs(servers) do
	if type(config) == 'function' then
		config = config()
	end

	if config.prefer_null_ls then
		if config.on_attach then
			local config_on_attach = config.on_attach;
			config.on_attach = function(client, bufnr)
				config_on_attach(client, bufnr);
				prefer_null_ls_fmt(client, bufnr);
			end
		else
			config.on_attach = prefer_null_ls_fmt
		end
	else
		if config.on_attach then
			local config_on_attach = config.on_attach;
			config.on_attach = function(client, bufnr)
				config_on_attach(client, bufnr);
				on_attach(client, bufnr);
			end
		else
			config.on_attach = on_attach
		end
	end

	config.capabilities = vim.tbl_deep_extend('keep', config.capabilities or {}, client_capabilities)

	lspconfig[server].setup(config)
end

-- null-ls setup
local null_fmt = null_ls.builtins.formatting
-- local null_diag = null_ls.builtins.diagnostics
-- local null_act = null_ls.builtins.code_actions
null_ls.setup {
	sources = {
		null_fmt.prettierd
	},
	on_attach = on_attach,
}

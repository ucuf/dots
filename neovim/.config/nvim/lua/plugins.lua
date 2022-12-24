vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerCompile
  augroup end
]])

local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
  vim.cmd [[packadd packer.nvim]]
end

return require('packer').startup(function(use)
	-- Packer can manage itself
	use 'wbthomason/packer.nvim'

	-- Quickstart configurations for the Nvim LSP client
	use {
		'neovim/nvim-lspconfig',
		requires = {
			-- This plugin adds vscode-like pictograms to neovim built-in lsp
			'onsails/lspkind.nvim',
			'p00f/clangd_extensions.nvim',
			{
				'jose-elias-alvarez/null-ls.nvim',
				requires = {"nvim-lua/plenary.nvim"},
			}
		}
	}

	use {
		'hrsh7th/nvim-cmp',
		requires = {
			'L3MON4D3/LuaSnip',
			'hrsh7th/cmp-nvim-lsp',
			{'hrsh7th/cmp-buffer', after = 'nvim-cmp'},
			{'hrsh7th/cmp-nvim-lua', after = 'nvim-cmp'},
			{'saadparwaiz1/cmp_luasnip', after = 'nvim-cmp'},
			{'hrsh7th/cmp-cmdline', after = 'nvim-cmp'},
		},
		config = [[require('config.cmp')]],
		event = 'InsertEnter *',
	}


	use {
		'nvim-treesitter/nvim-treesitter',
		requires = {
			{'nvim-treesitter/playground', after = 'nvim-treesitter'},
		},
		run = ':TSUpdate',
	}

	use {
		'nvim-telescope/telescope.nvim',
		requires = {'nvim-lua/plenary.nvim'},
		config = [[require('config.telescope')]],
		setup = [[require('config.telescope_setup')]],
	}

	-- emmet-vim provides support for expanding abbreviations similar to emmet
	-- see https://emmet.io/.
	use {
		'mattn/emmet-vim',
		ft = {
			'html',
			'css',
			'javascriptreact',
			'javascript',
		},
	}

	use({
		"kylechui/nvim-surround",
		config = function()
			require("nvim-surround").setup()
		end
	})

	use {
		'numToStr/Comment.nvim',
		config = function()
			require('Comment').setup()
		end
	}

	use 'junegunn/vim-easy-align'

	use {
		'NvChad/nvim-colorizer.lua',
		ft = {'css', 'javascript', 'vim', 'html', 'yaml'},
		-- config = [[require('colorizer').setup {'css', 'javascript', 'vim', 'html', 'yaml'}]],
		config = function()
			require('colorizer').setup({
				filetypes = { "*" },
				user_default_options = {
					RGB = true, -- #RGB hex codes
					RRGGBB = true, -- #RRGGBB hex codes
					names = true, -- "Name" codes like Blue or blue
					RRGGBBAA = false, -- #RRGGBBAA hex codes
					AARRGGBB = false, -- 0xAARRGGBB hex codes
					rgb_fn = false, -- CSS rgb() and rgba() functions
					hsl_fn = false, -- CSS hsl() and hsla() functions
					css = true, -- Enable all CSS features: rgb_fn, hsl_fn, names, RGB, RRGGBB
					css_fn = true, -- Enable all CSS *functions*: rgb_fn, hsl_fn
					-- Available modes for `mode`: foreground, background,  virtualtext
					mode = "background", -- Set the display mode.
					-- Available methods are false / true / "normal" / "lsp" / "both"
					-- True is same as normal
					tailwind = false, -- Enable tailwind colors
					-- parsers can contain values used in |user_default_options|
					sass = { enable = false, parsers = { css }, }, -- Enable sass colors
					virtualtext = "■",
				},
				-- all the sub-options of filetypes apply to buftypes
				buftypes = {},
			})
		end,
	}

	use '~/Programming/myprojects/darkside/'

	-- Testing plugins
	use {
		{
			'sheerun/vim-polyglot',
			cond = false,
		},
		{'ctrlpvim/ctrlp.vim'},
		{'nanotech/jellybeans.vim'},
		{'catppuccin/nvim', as = "catppuccin"},
	}

	use {
		'nvim-orgmode/orgmode',
		ft = {'org'},
		config = function()
			require('orgmode').setup_ts_grammar()
			require('orgmode').setup{}
		end
	}

	if packer_bootstrap then
		require('packer').sync()
	end

end)

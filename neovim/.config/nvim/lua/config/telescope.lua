local telescope = require 'telescope'
local actions = require "telescope.actions"
local actions_layout = require "telescope.actions.layout"

telescope.setup {
	defaults = {
		layout_strategy = 'bottom_pane',
		layout_config = {
			prompt_position = 'bottom',
		},
		border = false,
		mappings = {
			n = {
				["<M-p>"] = actions_layout.toggle_preview,
			},
		},
	},
	pickers = {
		find_files = {
			theme = "ivy",
			previewer = false,
		},
		buffers = {
			theme = "ivy",
			sort_mru = true,
			previewer = false,
			mappings = {
				i = { ["<c-d>"] = actions.delete_buffer },
			},
		},
		man_pages = {
			theme = "ivy",
			previewer = false,
		},
	},
}

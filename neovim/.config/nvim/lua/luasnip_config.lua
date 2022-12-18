local ls = require "luasnip"
local types = require "luasnip.util.types"

-- require("luasnip.loaders.from_lua").load({paths = "~/.config/nvim/snippets/"})

ls.config.set_config({
	history = true,
	updateevent = "TextChanged,TextChangedI",
	enable_autosnippets = true,
})

vim.keymap.set({"i", "s"}, "<c-k>", function()
	if ls.expand_or_jumpable() then
		ls.jump(-1)
	end
end);

vim.keymap.set({"i", "s"}, "<c-j>", function()
	if ls.expand_or_jumpable() then
		ls.expand_or_jump(1)
	end
end);

vim.keymap.set({"i", "s"}, "<c-l>", function()
	if ls.choice_active() then
		ls.change_choice(1)
	end
end);

vim.keymap.set({"i", "s"}, "<c-h>", function()
	if ls.choice_active() then
		ls.change_choice(-1)
	end
end);

-- vim.keymap.set({ "i", "s" }, "<C-i>", function() require'luasnip'.jump(1) end, { desc = "LuaSnip forward jump" })
-- vim.keymap.se({ "i", "s" }, "<M-i>", function() require'luasnip'.jump(-1) end, { desc = "LuaSnip backward jump" })


-- some shorthands...
local snip = ls.snippet
local node = ls.snippet_node
local text = ls.text_node
local insert = ls.insert_node
local func = ls.function_node
local choice = ls.choice_node
local dynamicn = ls.dynamic_node

local date = function()
	return {os.date('%Y-%m-%d')}
end


ls.add_snippets(nil, {
    all = {
        snip({
            trig = "date",
            namr = "Date",
            dscr = "Date in the form of YYYY-MM-DD",
        }, {
            func(date, {}),
        }),
	snip({
		trig = "meta",
		namr = "Metadata",
		dscr = "Yaml metadata format for markdown"
	},
	{
		text({"---",
		"title: "}), insert(1, "note_title"), text({"",
		"author: "}), insert(2, "author"), text({"",
		"date: "}), func(date, {}), text({"",
		"categories: ["}), insert(3, ""), text({"]",
		"lastmod: "}), func(date, {}), text({"",
		"tags: ["}), insert(4), text({"]",
		"comments: true",
		"---", ""}),
		insert(0)
	}),
    },
})

-- local function prequire(...)
-- local status, lib = pcall(require, ...)
-- if (status) then return lib end
--     return nil
-- end
-- 
-- local luasnip = prequire "luasnip"
-- local cmp = prequire("cmp")
-- 
-- luasnip.snippets = {
-- 	all = {
-- 		luasnip.parser.parse_snippet("expand", "this"),
-- 	},
-- }
-- 
-- local t = function(str)
--     return vim.api.nvim_replace_termcodes(str, true, true, true)
-- end
-- 
-- local check_back_space = function()
--     local col = vim.fn.col('.') - 1
--     if col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') then
--         return true
--     else
--         return false
--     end
-- end
-- 
-- _G.tab_complete = function()
--     if cmp and cmp.visible() then
--         cmp.select_next_item()
--     elseif luasnip and luasnip.expand_or_jumpable() then
--         return t("<Plug>luasnip-expand-or-jump")
--     elseif check_back_space() then
--         return t "<Tab>"
--     else
--         cmp.complete()
--     end
--     return ""
-- end
-- _G.s_tab_complete = function()
--     if cmp and cmp.visible() then
--         cmp.select_prev_item()
--     elseif luasnip and luasnip.jumpable(-1) then
--         return t("<Plug>luasnip-jump-prev")
--     else
--         return t "<S-Tab>"
--     end
--     return ""
-- end
-- 
-- vim.api.nvim_set_keymap("i", "<Tab>", "v:lua.tab_complete()", {expr = true})
-- vim.api.nvim_set_keymap("s", "<Tab>", "v:lua.tab_complete()", {expr = true})
-- vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
-- vim.api.nvim_set_keymap("s", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
-- vim.api.nvim_set_keymap("i", "<C-E>", "<Plug>luasnip-next-choice", {})
-- vim.api.nvim_set_keymap("s", "<C-E>", "<Plug>luasnip-next-choice", {})

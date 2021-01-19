local U = require "snippets.common"
local splitter = require "snippets.splitter"
local concat = table.concat
local api = vim.api

-- {
--  evaluate_defaults = <function 1>,
--  evaluate_inputs = <function 2>,
--  evaluate_structure = <function 3>,
--  inputs = { {
--      default = "",
--      first_index = 4,
--      id = 1,
--      order = 1
--    }, {
--      default = <function 4>,
--      first_index = 2,
--      id = 2,
--      order = 2
--    },
--    <metatable> = <1>{
--      __newindex = <function 5>
--    }
--  },
--  structure = { "local\n", {
--      default = <function 4>,
--      id = 2,
--      is_input = true,
--      order = 2,
--      <metatable> = <2>{
--        __tostring = <function 6>
--      }
--    }, " = require '", {
--      default = "",
--      id = 1,
--      is_input = true,
--      order = 1,
--      <metatable> = <table 2>
--    }, "'\n", {
--      default = "",
--      id = 0,
--      is_input = false,
--      order = 0,
--      <metatable> = <table 2>
--    },
--    <metatable> = <table 1>
--  },
--  zero_index = 6
-- }

local function advance_cursor(text, row, col)
  local lines = vim.split(text, "\n", true)
  row = row + #lines - 1
  if #lines > 1 then
    col = 0
  end
  local last_line = lines[#lines]
  col = col + #last_line
  return row, col
end

local function entrypoint(structure)
  local evaluator = U.evaluate_snippet(structure)
  -- Do initialization and insert the text to the buffer here.

  local S = evaluator.evaluate_structure({})
  local body = concat(S)

  local win = api.nvim_get_current_win()
  local bufnr = api.nvim_get_current_buf()
  local row, col = unpack(api.nvim_win_get_cursor(win))
  local current_line = api.nvim_get_current_line()
  local lines = splitter("\n", true).collect(body)
  lines[1] = current_line:sub(1, col) .. lines[1]
  lines[#lines] = lines[#lines] .. current_line:sub(col + 1)
  api.nvim_buf_set_lines(bufnr, row - 1, row, false, lines)

  local ns = api.nvim_create_namespace("baba-snippets")
  local defaults = evaluator.evaluate_inputs {}

  -- key=snippets id, value=extmark id
  local marks = {}

  local trow, tcol = row, col
  for i, v in pairs(evaluator.structure) do
    if type(v) == "string" then
      trow, tcol = advance_cursor(v, trow, tcol)
    elseif v.is_input then
      marks[v.id] = api.nvim_buf_set_extmark(bufnr, ns, trow - 1, tcol, {end_line = trow - 1, end_col = tcol})

      trow, tcol = advance_cursor(defaults[v.id] or "", trow, tcol)
    else
      trow, tcol = advance_cursor(S[i] or "", trow, tcol)
    end
  end
  marks["end"] = api.nvim_buf_set_extmark(bufnr, ns, trow - 1, tcol, {})

  local resolved_inputs = defaults
  local current_index = 0

  local R
  R = {
    aborted = false,
    -- - If there's nothing to advance, we should jump to the $0.
    -- - If there is no $0 in the structure/variables, we should
    -- jump to the end of insertion.
    advance = function(offset)
      offset = offset or 1
      current_index = math.max(math.min(current_index + offset, #evaluator.inputs + 1), 0)

      if current_index == 0 then
        R.aborted = true
        return true
      end

      -- Finished case
      if current_index > #evaluator.inputs then
        if marks[0] then
          local crow, ccol = unpack(api.nvim_buf_get_extmark_by_id(bufnr, ns, marks[0], {}))
          api.nvim_win_set_cursor(win, {crow + 1, ccol})
        else
          local crow, ccol = unpack(api.nvim_buf_get_extmark_by_id(bufnr, ns, marks["end"], {}))
          api.nvim_win_set_cursor(win, {crow + 1, ccol})
        end
        for _, mark in pairs(marks) do
          api.nvim_buf_del_extmark(bufnr, ns, mark)
        end
        return true
      end

      if current_index > 1 then
        local prev_mark = marks[current_index - 1]
        local crow, ccol, cdetails = unpack(api.nvim_buf_get_extmark_by_id(bufnr, ns, prev_mark, {details = true}))
        if cdetails.end_row < crow or cdetails.end_col < ccol then
          api.nvim_buf_set_extmark(bufnr, ns, cdetails.end_row, cdetails.end_col,
                                   {end_line = crow, end_col = ccol, id = prev_mark})
        end

        -- TODO(babariviere): fill resolved
      end

      local input = evaluator.inputs[current_index]
      local mark = marks[input.id]

      local crow, ccol = unpack(api.nvim_buf_get_extmark_by_id(bufnr, ns, mark, {}))
      crow = crow + 1 -- extmark is zero indexed
      api.nvim_win_set_cursor(win, {crow, ccol})
      local resolved = resolved_inputs[input.id] or ""

      if resolved ~= "" then
        -- TODO: prefill resolved
        api.nvim_feedkeys(api.nvim_replace_termcodes("<Esc>", true, false, true), "n", true)
        api.nvim_command [[normal! v]]
        crow, ccol = advance_cursor(resolved, crow, ccol)
        api.nvim_win_set_cursor(win, {crow, ccol})
        api.nvim_feedkeys(api.nvim_replace_termcodes("<C-g>", true, false, true), "m", true)
      end
    end
  }
  return R
end

return entrypoint

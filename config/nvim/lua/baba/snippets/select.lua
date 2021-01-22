local U = require "snippets.common"
local splitter = require "snippets.splitter"
local concat = table.concat
local api = vim.api

local function feedkeys(keys)
  api.nvim_feedkeys(api.nvim_replace_termcodes(keys, true, false, true), "n", true)
end

local function get_text(bufnr, start_line, start_col, end_line, end_col)
  local lines = api.nvim_buf_get_lines(bufnr, start_line, end_line + 1, false)
  lines[#lines] = lines[#lines]:sub(1, end_col + 1)
  lines[1] = lines[1]:sub(start_col + 1)
  return lines
end

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

  local ns = api.nvim_create_namespace("snippets.select")
  local defaults = evaluator.evaluate_inputs {}

  -- key=snippets id, value=extmark id
  local marks = {}

  -- create marks for each input fields
  local trow, tcol = row, col
  for i, v in pairs(evaluator.structure) do
    if type(v) == "string" then
      trow, tcol = advance_cursor(v, trow, tcol)
    elseif v.is_input or v["id"] == 0 then
      local start_line, start_col = trow - 1, tcol
      local end_row, end_col = advance_cursor(defaults[v.id] or "", trow, tcol)
      marks[v.id] = api.nvim_buf_set_extmark(bufnr, ns, start_line, start_col, {
        end_line = end_row - 1,
        end_col = end_col,
        right_gravity = false,
        end_right_gravity = true
      })

      trow, tcol = end_row, end_col
    else
      trow, tcol = advance_cursor(S[i] or "", trow, tcol)
    end
  end
  -- create the "cursor" mark if there is no $0 in macro
  if not marks[0] then
    marks["end"] = api.nvim_buf_set_extmark(bufnr, ns, trow - 1, tcol, {})
  end

  local resolved_inputs = {}
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

      -- close popup window (this causes some error with extmarks, don't know why)
      -- related: https://github.com/neovim/neovim/issues/13816
      if vim.fn.pumvisible() ~= 0 then
        feedkeys("<C-g><C-g>")
      end

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
      -- TODO(babariviere): use CursorMovedI for live update

      -- Resolve previous value, only if the user goes forward in snippet
      if current_index > 1 and offset > 0 then
        -- Reverse end and start when end < start
        local prev_mark = marks[current_index - 1]
        local pos = api.nvim_buf_get_extmark_by_id(bufnr, ns, prev_mark, {details = true})

        local prev_input = evaluator.inputs[current_index - 1]
        resolved_inputs[prev_input.id] = table.concat(
                                           get_text(bufnr, pos[1], pos[2], pos[3].end_row, pos[3].end_col - 1), "\n")
        local new_resolved = evaluator.evaluate_inputs(resolved_inputs)
        for i = current_index, #evaluator.inputs, 1 do
          local input = evaluator.inputs[i]
          local cur_value = resolved_inputs[input.id]
          local new_value = new_resolved[input.id]
          if new_value ~= cur_value and new_value ~= defaults[input.id] then
            resolved_inputs[input.id] = new_value

            local new_lines = vim.split(new_value, "\n", false)
            pos = api.nvim_buf_get_extmark_by_id(bufnr, ns, marks[input.id], {details = true})
            api.nvim_buf_set_text(bufnr, pos[1], pos[2], pos[3].end_row, pos[3].end_col, new_lines)
            local end_line, end_col
            if #new_lines > 1 then
              end_line = pos[3].end_row + #new_lines
              local last_line = new_lines[#new_lines]
              end_col = #last_line
            else
              local last_line = new_lines[#new_lines]
              end_col = pos[3].end_col + #last_line
            end
            api.nvim_buf_set_extmark(bufnr, ns, pos[1], pos[2], {
              end_line = end_line,
              end_col = end_col,
              id = marks[input.id],
              right_gravity = false,
              end_right_gravity = true
            })
          end
        end
      end

      local input = evaluator.inputs[current_index]
      local mark = marks[input.id]
      local pos = api.nvim_buf_get_extmark_by_id(bufnr, ns, mark, {details = true})
      api.nvim_win_set_cursor(win, {pos[1] + 1, pos[2]}) -- extmark is zero indexed
      local resolved = resolved_inputs[input.id] or defaults[input.id] or ""

      if resolved ~= "" then
        -- enter select mode
        feedkeys("<Esc>")
        api.nvim_command [[normal! v]]
        api.nvim_win_set_cursor(win, {pos[3].end_row + 1, pos[3].end_col})
        feedkeys("<C-g>")
      end
    end
  }
  return R
end

return entrypoint

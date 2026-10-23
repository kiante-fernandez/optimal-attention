-- pandoc filter for the PLOS LaTeX build:
--   [@key] citations -> \cite{key,...}; display math with \tag -> numbered equation env;
--   byline div dropped (the template carries the author block).
local in_latex = FORMAT:match('latex') ~= nil
function Cite(el)
  if not in_latex then return nil end
  local keys = {}
  for _, c in ipairs(el.citations) do table.insert(keys, c.id) end
  local suffix = ''
  local c1 = el.citations[1]
  if c1.suffix and #c1.suffix > 0 then suffix = '[' .. pandoc.utils.stringify(c1.suffix):gsub('^,%s*', '') .. ']' end
  return pandoc.RawInline('latex', '\\cite' .. suffix .. '{' .. table.concat(keys, ',') .. '}')
end
function Math(el)
  if not in_latex or el.mathtype ~= 'DisplayMath' then return nil end
  local body, n = el.text:gsub('\\tag{%d+}', '')
  local env = (n > 0) and 'equation' or 'equation*'      -- only \tag-ged displays are numbered
  return pandoc.RawInline('latex', '\\begin{' .. env .. '}' .. body .. '\\end{' .. env .. '}')
end
function Div(el)
  if in_latex and el.classes:includes('byline') then return {} end
  return nil
end

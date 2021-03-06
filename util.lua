local exports = { }

local function dump(node, level)
   if not level then level = 1 end
   if type(node) == 'nil' then
      return "null"
   end
   if type(node) == "string" then
      return '"'..node..'"'
   end
   if type(node) == "number" then
      return node
   end
   if type(node) == "boolean" then
      return tostring(node)
   end

   local buff = { }
   local dent = string.rep("    ", level)
   local tput = table.insert

   if #node == 0 and next(node, nil) then
      tput(buff, "{")
      local i_buff = { }
      local p_buff = { }
      for k,data in pairs(node) do
         tput(buff, "\n"..dent..dump(k)..': '..dump(data, level + 1))
         if next(node, k) then
            tput(buff, ",")
         end
      end
      tput(buff, "\n"..string.rep("    ", level - 1).."}")
   else
      tput(buff, "[")
      for i,data in pairs(node) do
         tput(buff, "\n"..dent..dump(data, level + 1))
         if i ~= #node then
            tput(buff, ",")
         end
      end
      tput(buff, "\n"..string.rep("    ", level - 1).."]")
   end

   return table.concat(buff, "")
end

exports.dump = dump

local ID = 0
exports.genid = function()
   ID = ID + 1
   return '__'..ID
end

function exports.extend(base, with)
   with.__super = base
   with.__index = with
   return setmetatable(with, { __index = base, __call = base.__call })
end

return exports

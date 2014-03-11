local B = require('builder')
local util = require('util')

local match = { }

function match:Program(node)
   self.hoist = { }
   self.scope = { }
   local export = B.identifier('export')
   for i=1, #node.body do
      local stmt = self:get(node.body[i])
      self.scope[#self.scope + 1] = stmt
   end
   for i=#self.hoist, 1, -1 do
      table.insert(self.scope, 1, self.hoist[i])
   end
   return B.chunk(self.scope)
end
function match:ModuleDeclaration(node)
   local name = self:get(node.id)
   self.hoist[#self.hoist + 1] = B.localDeclaration({ name }, { })

   local outer_scope = self.scope
   local outer_hoist = self.hoist

   self.scope = { }
   self.hoist = { }

   local export = B.identifier('export')
   self.scope[#self.scope + 1] = B.localDeclaration({ export }, { B.table({}) })

   for i=1, #node.body do
      local stmt = self:get(node.body[i])
      self.scope[#self.scope + 1] = stmt
   end
   for i=#self.hoist, 1, -1 do
      table.insert(self.scope, 1, self.hoist[i])
   end

   local body = self.scope

   self.scope = outer_scope
   self.hoist = outer_hoist

   body[#body + 1] = B.returnStatement({ export })

   return B.assignmentExpression({ name }, {
      B.callExpression(
         B.parenExpression{
            B.functionExpression({ }, B.blockStatement(body))
         }, { }
      )
   })
end
function match:Literal(node)
   return B.literal(node.value)
end
function match:Identifier(node)
   return B.identifier(node.name)
end
function match:VariableDeclaration(node)
   local init = node.init and self:get(node.init) or nil
   if node.export then
      local expr = B.memberExpression(B.identifier('export'), self:get(node.id))
      self.scope[#self.scope + 1] = B.assignmentExpression(
         { expr }, { init }
      )
      init = expr
   end
   return B.localDeclaration({ self:get(node.id) }, { init })
end
function match:AssignmentExpression(node)
   if #node.operator == 2 then
      local oper = string.sub(node.operator, 1, 1)
      return B.assignmentExpression({
         self:get(node.left)
      }, {
         B.binaryExpression(oper, self:get(node.left), self:get(node.right))
      })
   end
   return B.assignmentExpression({ self:get(node.left) }, { self:get(node.right) })
end
function match:MemberExpression(node)
   return B.memberExpression(
      self:get(node.object), self:get(node.property), node.computed
   )
end
function match:ThisExpression(node)
   return B.identifier('this')
end
function match:SuperExpression(node)
   return B.identifier('super')
end
function match:ReturnStatement(node)
   if self.retsig then
      return B.doStatement(
         B.blockStatement{
            B.assignmentExpression(
               { self.retsig }, { B.literal(true) }
            );
            B.assignmentExpression(
               { self.retval }, { self:get(node.argument) }
            );
            B.returnStatement{ self.retval }
         }
      )
   end
   return B.returnStatement{ self:get(node.argument) }
end
function match:IfStatement(node)
   local test, cons, altn = self:get(node.test)
   if node.consequent then
      cons = self:get(node.consequent)
   end
   if node.alternate then
      altn = self:get(node.alternate)
   end
   local stmt = B.ifStatement(test, cons, altn)
   return stmt
end

function match:TryStatement(node)
   local oldret = self.retsig
   local oldval = self.retval

   self.retsig = B.tempid()
   self.retval = B.tempid()

   local try = B.functionExpression({ }, self:get(node.body))

   local finally
   if node.finalizer then
      finally = B.functionExpression({ }, self:get(node.finalizer))
   end

   local clauses = { }
   for i=#node.guardedHandlers, 1, -1 do
      local clause = node.guardedHandlers[i]
      local cons = self:get(clause.body)
      local head = B.localDeclaration(
         { self:get(clause.param) }, { B.vararg() }
      )
      cons.body[#cons.body + 1] = B.breakStatement()
      clauses[#clauses + 1] = head
      clauses[#clauses + 1] = B.ifStatement(self:get(clause.guard), cons)
   end
   if node.handler then
      local clause = node.handler
      local cons = self:get(clause.body)
      local head = B.localDeclaration(
         { self:get(clause.param) }, { B.vararg() }
      )
      cons.body[#cons.body + 1] = B.breakStatement()
      clauses[#clauses + 1] = head
      clauses[#clauses + 1] = B.doStatement(cons)
   end

   local catch = B.functionExpression(
      { B.vararg() }, B.blockStatement{
         B.repeatStatement(B.literal(true), B.blockStatement(clauses))
      }
   )

   local expr = B.callExpression(B.identifier('try'), { try, catch, finally })
   local temp = self.retval
   local rets = self.retsig

   self.retsig = oldret
   self.retval = oldval

   return B.doStatement(
      B.blockStatement{
         B.localDeclaration({ rets }, { B.literal(false) });
         B.localDeclaration({ temp }, { B.literal(nil) });
         B.expressionStatement(expr);
         B.ifStatement(
            rets, B.blockStatement{ B.returnStatement{ temp } }
         )
      }
   )
end
function match:SwitchStatement(node)
   local body = { }
   local disc = B.tempid()

   local cases = { }
   for i=1, #node.cases do
      local case = node.cases[i]
      local test
      if case.test then
         test = B.binaryExpression("==", disc, self:get(case.test))
      else
         test = B.literal(true)
      end
      cases[#cases + 1] = B.ifStatement(
         test, B.blockStatement(self:list(case.consequent))
      )
   end
   body[#body + 1] = B.localDeclaration({ disc }, { self:get(node.discriminant) })
   body[#body + 1] = B.repeatStatement(
      B.literal(true), B.blockStatement(cases)
   )
   return B.doStatement(B.blockStatement(body))
end
function match:BreakStatement(node)
   return B.breakStatement()
end
function match:ThrowStatement(node)
   return B.expressionStatement(
      B.callExpression(B.identifier('throw'), { self:get(node.argument) }) 
   )
end

function match:LogicalExpression(node)
   local o = node.operator
   if o == '&&' then
      o = 'and'
   elseif o == '||' then
      o = 'or'
   end
   return B.logicalExpression(o, self:get(node.left), self:get(node.right))
end

local bitop = {
   [">>"]  = 'rshift',
   [">>>"] = 'arshift',
   ["<<"]  = 'lshift',
   ["|"]   = 'bor',
   ["&"]   = 'band',
   ["^"]   = 'bxor',
}
function match:BinaryExpression(node)
   local o = node.operator
   if bitop[o] then
      local call = B.memberExpression(
         B.identifier('bit'),
         B.identifier(bitop[o])
      )
      local args = { self:get(node.left), self:get(node.right) }
      return B.callExpression(call, args)
   elseif o == 'instanceof' then
      return B.callExpression(
         B.identifier('instanceof'), { self:get(node.left), self:get(node.right) }
      )
   end
   if o == '~' then o = '..' end
   return B.binaryExpression(o, self:get(node.left), self:get(node.right))
end
function match:FunctionDeclaration(node)
   local name
   if not node.expression then
      name = self:get(node.id)
   end

   local params  = { }
   local prelude = { }
   local vararg  = false

   prelude[#prelude + 1] = B.localDeclaration(
      { B.identifier('this') }, {
      B.logicalExpression("or", B.identifier('self'), B.identifier('this'))
   })

   for i=1, #node.params do
      params[#params + 1] = self:get(node.params[i])
      if node.defaults[i] then
         local name = self:get(node.params[i])
         local test = B.binaryExpression("==", name, B.literal(nil))
         local expr = self:get(node.defaults[i])
         local cons = B.blockStatement{
            B.assignmentExpression({ name }, { expr })
         }
         prelude[#prelude + 1] = B.ifStatement(test, cons)
      end
   end

   if node.rest then
      params[#params + 1] = B.vararg()
      prelude[#prelude + 1] = B.localDeclaration(
         { B.identifier(node.rest.name) },
         { B.callExpression(
            B.identifier('new'),
            { B.identifier('Array'), B.vararg() }
         ) }
      )
   end

   local body = self:get(node.body)
   for i=#prelude, 1, -1 do
      table.insert(body.body, 1, prelude[i])
   end

   if node.expression then
      return B.functionExpression(params, body, vararg)
   else
      return B.functionDeclaration(name, params, body, vararg)
   end
end
--[[
   local Point = class("Point", function(this, super)
      Object:defineProperties(this, {
         move = {
            value = function(self, x, y)

            end,
         }
      })
   end)
]]
function match:ClassDeclaration(node)
   local name = self:get(node.id)
   local base = node.base and self:get(node.base) or B.identifier('Object')

   local properties = { }

   for i=1, #node.body do
      local prop = node.body[i]
      local desc = properties[prop.key.name] or { }
      if prop.kind == 'get' then
         desc.get = self:get(prop)
      elseif prop.kind == 'set' then
         desc.set = self:get(prop)
      else
         desc.value = self:get(prop)
      end
      if desc.static then
         if desc.static.value ~= prop.static then
            error("property "..prop.key.name.." already defined as static")
         end
      end
      desc.static = B.literal(prop.static)
      properties[prop.key.name] = desc
   end

   for k,v in pairs(properties) do
      properties[k] = B.table(v)
   end

   local body = {
      B.expressionStatement(B.sendExpression(
            B.identifier("Object"),
            B.identifier("defineProperties"),
            { B.identifier("this"), B.table(properties) }
         )
      )
   }
   self.hoist[#self.hoist + 1] = B.localDeclaration({ name }, { })

   local init = B.callExpression(
      B.identifier('class'), {
         B.literal(node.id.name), base,
         B.functionExpression(
            { B.identifier('this'), B.identifier('super') },
            B.blockStatement(body)
         )
      }
   )

   if node.export then
      local expr = B.memberExpression(B.identifier('export'), name)
      self.scope[#self.scope + 1] = B.assignmentExpression(
         { expr }, { init }
      )
      init = expr
   end

   return B.assignmentExpression(
      { name }, { init }
   )
end
function match:SpreadExpression(node)
   return B.callExpression(
      B.identifier('__spread__'), { self:get(node.argument) }
   )
end
function match:NullExpression(node)
   return B.identifier('null')
end
function match:PropertyDefinition(node)
   return self:get(node.value)
end
function match:BlockStatement(node)
   return B.blockStatement(self:list(node.body))
end
function match:ExpressionStatement(node)
   return B.expressionStatement(self:get(node.expression))
end
function match:CallExpression(node)
   local callee = node.callee
   if callee.type == 'MemberExpression' and not callee.computed then
      if callee.object.type == 'SuperExpression' then
         local args = self:list(node.arguments)
         local recv = B.memberExpression(
            B.memberExpression(
               B.identifier('super'), B.identifier('__methods__')
            ),
            self:get(callee.property)
         )
         table.insert(args, 1, B.identifier('this'))
         return B.callExpression(recv, args)
      else
         local recv = self:get(callee.object)
         local prop = self:get(callee.property)
         return B.sendExpression(recv, prop, self:list(node.arguments))
      end
   else
      if callee.type == 'SuperExpression' then
         local args = self:list(node.arguments)
         local recv = B.memberExpression(
            B.memberExpression(
               B.identifier('super'), B.identifier('__methods__')
            ),
            B.identifier('constructor')
         )
         table.insert(args, 1, B.identifier('this'))
         return B.callExpression(recv, args)
      else
         local args = self:list(node.arguments)
         table.insert(args, 1, B.literal(nil))
         return B.callExpression(self:get(callee), args)
      end
   end
end
function match:NewExpression(node)
   return B.callExpression(B.identifier('new'), {
      self:get(node.callee), unpack(self:list(node.arguments))
   })
end
function match:WhileStatement(node)
   local body = self:get(node.body)

   local test= self:get(node.test)

   return B.whileStatement(test, body)
end
function match:ForStatement(node)
   local body = self:get(node.body)
   if node.post[1] then
      body.body[#body.body + 1] = self:get(node.post[1]);
   end

   local init
   if node.init[1] then
      init = self:get(node.init[1])
   else
      init = B.emptyStatement()
   end

   local test
   if node.test[1] then
      test = self:get(node.test[1])
   else
      test = B.literal(true)
   end

   return B.doStatement(
      B.blockStatement{ init, B.whileStatement(test, body) }
   )
end
function match:ArrayExpression(node)
   local args = { B.identifier('Array') }
   for i=1, #node.elements do
      args[#args + 1] = self:get(node.elements[i])
   end
   return B.callExpression(B.identifier('new'), args)
end
function match:ObjectExpression(node)
   local properties = { }
   for i=1, #node.properties do
      local prop = node.properties[i]

      local key, val
      if prop.key.type == 'Identifier' then
         key = prop.key.name
      elseif prop.key.type == "Literal" then
         key = prop.key.value
      end

      local desc = properties[key] or { }

      if prop.kind == 'get' then
         desc.get = self:get(prop.value)
      elseif prop.kind == 'set' then
         desc.set = self:get(prop.value)
      else
         desc.value = self:get(prop.value)
      end

      properties[key] = desc
   end

   for k,v in pairs(properties) do
      properties[k] = B.table(v)
   end

   return B.sendExpression(
      B.identifier('Object'), B.identifier("create"), {
         B.literal(nil);
         B.table(properties);
      }
   )
end
function match:ForOfStatement(node)
   local none = B.tempid()
   local temp = B.tempid()
   local iter = B.callExpression(B.identifier('pairs'), { self:get(node.right) })
   local left = self:get(node.left)
   local body = self:get(node.body)
   if node.left.type == 'VariableDeclaration' then
      left = self:get(node.left.id)
   else
      table.insert(body.body, 1, B.assignmentExpression({ left }, { temp }))
      left = temp
   end
   return B.forInStatement(B.forNames{ none, left }, iter, body)
end
function match:RegExp(node)
   return B.callExpression(
      B.identifier('new'), {
         B.identifier('RegExp'),
         B.literal(node.pattern),
         B.literal(node.flags)
      }
   )
end
function match:RawString(node)
   local list = { }
   local tostring = B.identifier('tostring')
   for i=1, #node.expressions do
      local expr = node.expressions[i]
      if type(expr) == 'string' then
         list[#list + 1] = B.literal(expr)
      else
         list[#list + 1] = B.callExpression(tostring, { self:get(expr.expression) })
      end
   end
   return B.listExpression('..', list)
end
function match:ArrayComprehension(node)
   local temp = B.tempid()
   local body = B.blockStatement{
      B.localDeclaration({ temp }, {
         B.callExpression(
            B.identifier('new'), {  B.identifier('Array') }
         )
      })
   }
   local last = body
   for i=1, #node.blocks do
      local loop = self:get(node.blocks[i])
      last.body[#last.body + 1] = loop
      last = loop.body
   end
   last.body[#last.body + 1] = B.assignmentExpression({
      B.memberExpression(temp, B.unaryExpression('#', temp), true)
   }, {
      self:get(node.body)    
   })
   body.body[#body.body + 1] = B.returnStatement{ temp }
   return B.callExpression(
      B.parenExpression{
         B.functionExpression({ }, body)
      }, { }
   )
end
function match:ComprehensionBlock(node)
   local none = B.tempid()
   local temp = B.tempid()
   local iter = B.callExpression(B.identifier('pairs'), { self:get(node.right) })
   local left = self:get(node.left)
   local body = { }
   if node.left.type == 'Identifier' then
      left = self:get(node.left)
   else
      table.insert(body, 1, B.assignmentExpression({ left }, { temp }))
      left = temp
   end
   return B.forInStatement(B.forNames{ none, left }, iter, B.blockStatement(body))
end

local function transform(tree, src)
   local self = { }
   self.line = 0

   function self:sync(node)
      -- noop for now, line info needs to be fixed in the parser
   end

   function self:get(node)
      if not match[node.type] then
         error("no handler for "..tostring(node.type))
      end
      self:sync(node)
      local out = match[node.type](self, node)
      out.line = self.line
      return out
   end

   function self:list(nodes)
      local list = { }
      for i=1, #nodes do
         list[#list + 1] = self:get(nodes[i])
      end
      return list
   end

   return self:get(tree)
end

return {
   transform = transform
}

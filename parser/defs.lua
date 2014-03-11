local defs = { }
defs.tonumber = function(s)
   local n = string.gsub(s, '_', '')
   return tonumber(n)
end
defs.tostring = tostring

function defs.octal(s)
   return tostring(tonumber(s, 8))
end
function defs.quote(s)
   return string.format("%q", s)
end
function defs.program(body)
   return { type = "Program", body = body }
end
function defs.moduleDecl(name, body)
   return { type = "ModuleDeclaration", id = name, body = body }
end
function defs.exportDecl(stmt)
   stmt.export = true
   return stmt
end
function defs.rawString(exprs)
   return { type = "RawString", expressions = exprs }
end
function defs.rawExpr(expr)
   return { type = "RawExpression", expression = expr }
end
function defs.importStmt(names, from)
   return { type = "ImportStatement", names = names, from = from }
end
function defs.error(src, pos)
   local loc = string.sub(src, pos, pos)
   if loc == '' then
      error("Unexpected end of input")
   else
      local tok = string.match(src, '(%w+)', pos) or loc
      error("Unexpected token '"..tok.."'")
   end
end
local strEscape = {
   ["\\r"] = "\r",
   ["\\n"] = "\n",
   ["\\t"] = "\t",
   ["\\\\"] = "\\",
}
function defs.string(str)
   return string.gsub(str, "(\\[rnt\\])", strEscape)
end
function defs.literal(val)
   return { type = "Literal", value = val }
end
function defs.boolean(val)
   return val == 'true'
end
function defs.identifier(name)
   return { type = "Identifier", name = name }
end
function defs.compExpr(bs, b, f)
   return { type = "ArrayComprehension", blocks = bs, body = b, filter = f }
end
function defs.compBlock(lhs, rhs)
   return { type = "ComprehensionBlock", left = lhs, right = rhs }
end
function defs.arrayExpr(elements)
   return { type = "ArrayExpression", elements = elements }
end
function defs.objectExpr(properties)
   return { type = "ObjectExpression", properties = properties }
end
function defs.arrayPatt(elements)
   return { type = "ArrayPattern", elements = elements }
end
function defs.objectPatt(properties)
   return { type = "ObjectPattern", properties = properties }
end
function defs.objectMember(prop)
   if prop.type ~= "PropertyDefinition" then
      prop.kind = "init"
   end
   return prop
end
function defs.ifStmt(test, cons, altn)
   if cons.type ~= "BlockStatement" then
      cons = defs.blockStmt{ cons }
   end
   if altn and altn.type ~= "BlockStatement" then
      altn = defs.blockStmt{ altn }
   end
   return { type = "IfStatement", test = test, consequent = cons, alternate = altn }
end
function defs.whileStmt(test, body)
   return { type = "WhileStatement", test = test, body = body }
end
function defs.forStmt(init, test, post, body)
   return {
      type = "ForStatement", init = init, test = test, post = post, body = body
   }
end
function defs.forOfStmt(left, right, body)
   return { type = "ForOfStatement", left = left, right = right, body = body }
end
function defs.varDecl(name, init)
   return { type = "VariableDeclaration", id = name, init = init }
end
function defs.nullExpr()
   return { type = "NullExpression" }
end
function defs.spreadExpr(arg)
   return { type = "SpreadExpression", argument = arg }
end
function defs.regexExpr(expr, flags)
   local rx = require('pcre')
   expr = string.gsub(expr, "(\\[rnt\\])", strEscape)
   assert(rx.compile(expr))
   return { type = "RegExp", pattern = expr, flags = flags }
end
function defs.funcDecl(name, head, body)
   if body.type ~= "BlockStatement" then
      body = defs.blockStmt{ defs.exprStmt(body) }
   end
   local decl = { type = "FunctionDeclaration", id = name, body = body }
   local defaults, params, rest = { }, { defs.identifier('self') }, nil
   for i=1, #head do
      local p = head[i]
      if p.rest then
         rest = p.name
      else
         params[#params + 1] = p.name
         if p.default then
            defaults[i] = p.default
         end
      end 
   end
   decl.params   = params
   decl.defaults = defaults
   decl.rest     = rest
   return decl
end
function defs.funcExpr(head, body)
   local decl = defs.funcDecl(nil, head, body)
   decl.expression = true
   return decl
end
function defs.blockStmt(body)
   return {
      type = "BlockStatement",
      body = body
   }
end
function defs.returnStmt(expr)
   return { type = "ReturnStatement", argument = expr }
end
function defs.switchStmt(disc, cases, default)
   if default then
      cases[#cases + 1] = defs.switchCase(nil, default)
   end
   return { type = "SwitchStatement", discriminant = disc, cases = cases }
end
function defs.switchCase(test, cons)
   return { type = "SwitchCase", test = test, consequent = cons }
end
function defs.breakStmt()
   return { type = "BreakStatement" }
end
function defs.throwStmt(expr)
   return { type = "ThrowStatement", argument = expr }
end
function defs.tryStmt(body, handlers, finalizer)
   local guarded = { }
   local handler
   for i=1, #handlers do
      if handlers[i].guard then
         guarded[#guarded + 1] = handlers[i]
      else
         assert(i == #handlers, "catch all handler must be last")
         handler = handlers[i]
      end
   end
   return {
      type = "TryStatement",
      body = body,
      handler = handler,
      guardedHandlers = guarded,
      finalizer = finalizer
   }
end
function defs.catchClause(param, guard, body)
   if not body then body, guard = guard, nil end
   return { type = "CatchClause", param = param, guard = guard, body = body }
end
function defs.classDecl(name, base, body)
   if #base == 0 and not base.type then
      base = nil
   end
   return { type = "ClassDeclaration", id = name, base = base, body = body }
end
function defs.classMember(s, m)
   m.static = s == "static"
   return m
end
function defs.propDefn(k, n, h, b)
   return {
      type = "PropertyDefinition", kind = k, key = n, value = defs.funcExpr(h, b)
   }
end
function defs.exprStmt(expr)
   return { type = "ExpressionStatement", expression = expr }
end
function defs.thisExpr()
   return { type = "ThisExpression" }
end
function defs.superExpr()
   return { type = "SuperExpression" }
end
function defs.prefixExpr(o, a)
   return { type = "UnaryExpression", operator = o, argument = a }
end
function defs.postfixExpr(expr)
   local base = expr[1]
   for i=2, #expr do
      if expr[i][1] == "(" then
         base = defs.callExpr(base, expr[i][2])
      elseif expr[i][1] == '++' or expr[i][1] == '--' then
         base = defs.updateExpr(base, expr[i][2])
      else
         base = defs.memberExpr(base, expr[i][2], expr[i][1] == "[")
      end
   end
   return base
end
function defs.updateExpr(b, o)
   return { type = "UpdateExpression", argument = b, operator = o }
end
function defs.memberExpr(b, e, c)
   return { type = "MemberExpression", object = b, property = e, computed = c }
end
function defs.callExpr(expr, args)
   return { type = "CallExpression", callee = expr, arguments = args } 
end
function defs.newExpr(expr, args)
   return { type = "NewExpression", callee = expr, arguments = args } 
end

function defs.binaryExpr(op, lhs, rhs)
   return { type = "BinaryExpression", operator = op, left = lhs, right = rhs }
end
function defs.logicalExpr(op, lhs, rhs)
   return { type = "LogicalExpression", operator = op, left = lhs, right = rhs }
end
function defs.assignExpr(lhs, op, rhs)
   return { type = "AssignmentExpression", operator = op, left = lhs, right = rhs }
end

local prec = {
   ["||"] = 1,
   ["&&"] = 2,
   ["|"] = 3,
   ["^"] = 4,
   ["&"] = 5,

   ["=="] = 6,
   ["!="] = 6,
   ["==="] = 6,
   ["!=="] = 6,

   ["instanceof"] = 7,
   ["in"] = 8,

   [">="] = 9,
   ["<="] = 9,
   [">"] = 9,
   ["<"] = 9,

   ["<<"] = 10,
   [">>"] = 10,
   [">>>"] = 10,

   ["-"] = 11,
   ["+"] = 11,
   ["~"] = 11,

   ["*"] = 12,
   ["/"] = 12,
   ["%"] = 12,
}

local shift = table.remove

local function fold_infix(exp, lhs, min)
   while prec[exp[1]] ~= nil and prec[exp[1]] >= min do
      local op  = shift(exp, 1)
      local rhs = shift(exp, 1)
      while prec[exp[1]] ~= nil and prec[exp[1]] > prec[op] do
         rhs = fold_infix(exp, rhs, prec[exp[1]])
      end
      if op == "||" or op == "&&" then
         lhs = defs.logicalExpr(op, lhs, rhs)
      else
         lhs = defs.binaryExpr(op, lhs, rhs)
      end
   end
   return lhs
end

function defs.infixExpr(exp)
   return fold_infix(exp, shift(exp, 1), 0)
end

return defs

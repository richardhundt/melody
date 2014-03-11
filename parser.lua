local util = require("util")
local re   = require('re')
local defs = require('parser.defs')

local patt = [[
   program  <- {|
      s (<main_stmt> (<sep> s <main_stmt>)* <sep>?)? s (!. / '' => error)
   |} -> program

   lcomment <- (!%nl %s)* "//" (!%nl .)* %nl
   bcomment <- "/*" (!"*/" .)* "*/"
   comment  <- <lcomment> / <bcomment>
   idsafe   <- !(%alnum / "_")
   s        <- (<comment> / %s)*
   S        <- (<comment> / %s)+
   hs       <- !%nl %s
   digits   <- %digit (%digit / (&('_' %digit) '_') %digit)*
   word     <- (%alpha / "_") (%alnum / "_")*

   keyword  <- (
      "var" / "function" / "class" / "module" / "static"
      / "new" / "null" / "true" / "false" / "return" / "throw"
      / "yield" / "await" / "break" / "continue" / "switch"
      / "case" / "default" / "while" / "do" / "for" / "in" / "of"
      / "this" / "super" / "import" / "export" / "module"
      / "try" / "catch" / "finally" / "if" / "else"
      / "instanceof" / "typeof"
   ) <idsafe>

   sep     <- <bcomment>? (%nl / ";" / &"}" / <lcomment>) / %s <sep>?

   qstring <- '"' { (!'"' .)* } '"'
   astring <- "'" { (!"'" .)* } "'"

   special <- "\n" / "\$" / "\\" / "\" .

   rstring <- {|
      '`' (
         <raw_expr> / { (<special> / !(<raw_expr> / "`") .)+ }
      )* '`'
   |} -> rawString

   raw_expr <- (
      "${" s <expr> s "}"
   ) -> rawExpr

   string  <- (
      <qstring> / <astring>
   ) -> string

   hexadec <- "-"? "0x" %xdigit+

   decimal <- "-"? <digits> "." <digits> (("e"/"E") "-"? <digits>)? -> tonumber

   integer <- "-"? <digits>

   octal   <- {~ { "-"? "0" [0-7]+ } -> octal ~}

   number  <- {~
      <hexadec> / <octal> / <decimal> / <integer>
   ~} -> tonumber

   boolean <- (
      {"true"/"false"} <idsafe>
   ) -> boolean

   literal <- ( <number> / <string> / <boolean> ) -> literal

   main_stmt <- (
        <module_decl>
      / <import_stmt>
      / <export_decl>
      / <stmt>
   )

   module_decl <- (
      "module" <idsafe> s <ident> s "{" s
         {| (<main_stmt> (<sep> s <main_stmt>)*)? |} s
      "}"
   ) -> moduleDecl

   export_decl <- (
      "export" <idsafe> s (<decl_stmt> / <module_decl>)
   ) -> exportDecl

   import_stmt <- (
      "import" <idsafe> s {| {"*"} / "{" s <ident> (s "," s <ident>)* s "}" |} s
      "from" s <string>
   ) -> importStmt

   stmt <- (
      <if_stmt>
      / <while_stmt>
      / <for_stmt>
      / <for_of_stmt>
      / <expr_stmt>
      / <decl_stmt>
      / <return_stmt>
      / <break_stmt>
      / <throw_stmt>
      / <try_stmt>
      / <switch_stmt>
   )

   stmt_list <- {|
      (<stmt> (<sep> s <stmt>)* <sep>?)?
   |}

   switch_stmt <- (
      "switch" <idsafe> s "(" s <expr> s ")" s "{" s
         {| <switch_case>* |}
         (s "default" <idsafe> s ":" s (<block_stmt> / <stmt_list>))? s
      "}"
   ) -> switchStmt

   switch_case <- (
      s "case" <idsafe> s <expr> s ":" s (<block_stmt> / <stmt_list>)
   ) -> switchCase

   break_stmt <- (
      "break" <idsafe>
   ) -> breakStmt

   throw_stmt <- (
      "throw" <idsafe> s <expr>
   ) -> throwStmt

   try_stmt <- (
      "try" <idsafe> s <block_stmt>
      {| <catch_clause>* |} (s "finally" <idsafe> s <block_stmt>)?
   ) -> tryStmt

   catch_clause <- (
      s "catch" <idsafe> s "(" s
      <ident> (s "if" <idsafe> s <expr>)? s ")" s <block_stmt> s
   ) -> catchClause

   return_stmt <- (
      "return" <idsafe> s <expr>
   ) -> returnStmt

   decl_stmt <- (
      <var_decl> / <func_decl> / <class_decl>
   )

   var_decl <- (
      "var" <idsafe> <var_decl_item> (s "," s <var_decl_item>)*
   )

   var_decl_item <- (
       s <patt> (s "=" s <expr>)?
   ) -> varDecl

   patt <- (
      <array_patt> / <object_patt> / <member_expr>
   )

   array_patt <- (
      "[" s {| <patt> (s "," s <patt>)* |} "]"
   ) -> arrayPatt

   object_patt <- (
      "{" s {| <object_patt_pair> (s "," s <object_patt_pair>)* |} "}"
   ) -> objectPatt
   object_patt_pair <- (
      (<literal> / <ident>) s ":" s <patt>
   )

   func_decl <- (
      "function" <idsafe> s <ident> "(" s {| <param_list>? |} s ")" s <func_body>
   ) -> funcDecl

   func_expr <- (
      "function" <idsafe> s "(" s {| <param_list>? |} s ")" s <func_body>
      / "(" s {| <param_list>? |} s ")" s "=>" s <func_body>
      / {| {| {:name: <ident> :} |} |} s "=>" s <func_body>
   ) -> funcExpr

   func_body <- <block_stmt> / <expr>

   class_decl <- (
      "class" <idsafe> s <ident> (s <class_heritage>)? s "{" s <class_body> s "}"
   ) -> classDecl

   class_body <- {|
      (<class_member> (<sep> s <class_member>)* <sep>?)?
   |}

   class_member <- (
      ({"static"} <idsafe> s / '' -> "virtual") <prop_defn>
   ) -> classMember

   class_heritage <- (
      "extends" <idsafe> s <expr> / {| |}
   )

   prop_defn <- (
      ({"get"/"set"} s / '' -> "init") <ident> s
      "(" s {| <param_list>? |} s ")" s
      <func_body>
   ) -> propDefn

   param <- {|
      {:name: <ident> :} (s "=" s {:default: <expr> :})?
   |}
   param_list <- (
        <param> s "," s <param_list>
      / <param> s "," s <param_rest>
      / <param>
      / <param_rest>
   )

   param_rest <- {| "..." {:name: <ident> :} {:rest: '' -> 'true' :} |}

   block_stmt <- (
      "{" s {| (<stmt> (<sep> s <stmt>)* <sep>?)? |} s "}"
   ) -> blockStmt

   if_stmt <- (
      "if" <idsafe> s "(" s <expr> s ")" s <if_body>
      (s "else" <idsafe> s <if_stmt> / s "else" <idsafe> s <if_body>)?
   ) -> ifStmt

   if_body <- <block_stmt> / <expr_stmt>

   for_stmt <- (
      "for" <idsafe> s "(" s
      {| <for_init>?  |} s ";" s
      {| <expr>?      |} s ";" s
      {| <expr_stmt>? |} s ")" s
      <loop_body>
   ) -> forStmt
   for_init <- <var_decl> / <expr_stmt>

   for_of_stmt <- (
      "for" <idsafe> s "(" s <for_of_init> s "of" <idsafe> s <expr> s ")" s
      <loop_body>
   ) -> forOfStmt
   for_of_init <- <var_decl> / <patt>

   loop_body <- <block_stmt> / <expr_stmt>

   while_stmt <- (
      "while" <idsafe> s "(" s <expr> s ")" s <loop_body>
   ) -> whileStmt

   ident <- (
      !<keyword> { (%alpha / "_") (%alnum / "_")* }
   ) -> identifier

   term <- (
        <func_expr>
      / <null_expr>
      / <this_expr>
      / <super_expr>
      / <comp_expr>
      / <array_expr>
      / <object_expr>
      / <regex_expr>
      / <ident>
      / <literal>
      / <rstring>
      / "(" s <expr> s ")"
   )

   expr <- <infix_expr> / <new_expr> / <spread_expr>

   spread_expr <- (
      "..." <postfix_expr>
   ) -> spreadExpr

   this_expr <- (
      "this" <idsafe>
   ) -> thisExpr

   null_expr <- (
      "null" <idsafe>
   ) -> nullExpr

   super_expr <- (
      "super" <idsafe>
   ) -> superExpr

   expr_stmt <- (
      (<assign_expr> / !("{" / ("class" / "function")<idsafe>) <expr>)
   ) -> exprStmt

   binop <- {
      "+" / "-" / "~" / "/" / "*" / "%" / "^" / "||" / "&&"
      / "|" / "&" / ">>>" / ">>" / ">=" / ">" / "<<" / "<=" / "<"
      / "!==" / "===" / "!=" / "==" / "instanceof" <idsafe>
   }

   infix_expr  <- (
      {| <prefix_expr> (s <binop> s <prefix_expr>)+ |}
   ) -> infixExpr / <prefix_expr>

   prefix_expr <- (
      { "++" / "--" / "~" / "+" / "-" / "!" / "typeof" <idsafe> } s <prefix_expr>
   ) -> prefixExpr / <postfix_expr>

   postfix_expr <- {|
      <term> (s <postfix_tail>)+
   |} -> postfixExpr / <term>

   postfix_tail <- {|
        { "." } s <ident>
      / { "[" } s <expr> s ("]" / '' => error)
      / { "(" } s {| (<expr> (s "," s <expr>)*)? |} s (")" / '' => error)
      / { "++" / "--" }
   |}

   update_expr <- (
      <member_expr> {"++" / "--"}
   ) -> updateExpr

   member_expr <- {|
      <term> (s <member_next>)?
   |} -> postfixExpr / <term>
   member_next <- (
      <postfix_tail> s <member_next> / <member_tail>
   )
   member_tail <- {|
        { "." } s <ident>
      / { "[" } s <expr> s ("]" / '' => error)
   |}

   assop <- {
      "=" / "+=" / "-=" / "~=" / "*=" / "/=" / "%="
      / "|=" / "&=" / "^=" / "<<=" / ">>>=" / ">>="
   }

   assign_expr <- (
      <patt> s <assop> s <expr>
   ) -> assignExpr

   new_expr <- (
      "new" <idsafe> s <member_expr> (
         s "(" s {| (<expr> (s "," s <expr>)*)? |} s ")"
         / {| |}
      )
   ) -> newExpr

   array_expr <- (
      "[" s {| <array_elements>? |} s "]"
   ) -> arrayExpr

   array_elements <- <expr> (s "," s <expr>)* (s ",")?

   object_expr <- (
      "{" s {| <object_members>? |} s "}"
   ) -> objectExpr

   object_members <- (
      <object_member> (s "," s <object_member>)* (s ",")?
   )
   object_member <- (<prop_defn> / {|
      {:key: (<literal> / <ident>) :} s ":" s {:value: <expr> :}
   |}) -> objectMember

   comp_expr <- (
      "[" s {| <comp_block>+ |} <expr> (s "if" <idsafe> s "(" s <expr> s ")")? s "]"
   ) -> compExpr

   comp_block <- (
      "for" <idsafe> s "(" s <patt> s "of" <idsafe> s <expr> s ")" s
   ) -> compBlock

   regex_expr <- (
      "/" { ( "\\" / "\/" / !("/" / %nl) .)* } "/" {[gmi]*}
   ) -> regexExpr
]]

local grammar = re.compile(patt, defs)
local function parse(src)
   return grammar:match(src)
end

return {
   parse = parse
}



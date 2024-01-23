import Lean

def minput :=
"Time:      7  15   30
Distance:  9  40  200"

def input :=
"Time:        51     92     68     90
Distance:   222   2031   1126   122"

def parse (s:String):=
  let l := s.splitOn "\n"
    |>.map λ s => s.splitOn " "
      |>.filterMap String.toNat?
  if let [a,b] := l then
    some (List.zip a b)
  else none

#eval parse minput

def evalRace (r:Nat × Nat) :=
  List.range r.fst
  |>.tail!
  |>.map (λ n => n*(r.fst-n))
  |>.filter (· > r.snd)
  |>.length

#eval evalRace (7,9)

#eval if let some l := (parse minput) then some (l.map (λ r => evalRace r) |>.foldl (λ acc n => acc*n) 1) else none

def a := "{}"
#eval s!"hello{a}"

-- def sscanf (s:String) (f:String) : Option any :=

-- #eval sscanf!"hello {}

-- inductive Dyck : Type where
--   | round : Dyck → Dyck  -- ( <inner> )
--   | curly : Dyck → Dyck  -- { <inner> }
--   | leaf : Dyck

-- -- declare Dyck grammar parse trees
-- declare_syntax_cat brack
-- syntax "(" brack ")" : brack
-- syntax "{" brack "}" : brack
-- syntax "end" : brack

-- -- notation for translating `brack` into `term`
-- syntax "`[Dyck| " brack "]" : term

-- -- rules to translate Dyck grammar into inductive value of type Dyck
-- macro_rules
--   | `(`[Dyck| end])    => `(Dyck.leaf)
--   | `(`[Dyck| ($b)]) => `(Dyck.round `[Dyck| $b])  -- recurse
--   | `(`[Dyck| {$b}]) => `(Dyck.curly `[Dyck| $b])  -- recurse

-- -- tests
-- #check `[Dyck| end]      -- Dyck.leaf
-- #check `[Dyck| {(end)}]  -- Dyck.curl (Dyck.round Dyck.leaf)




-- -- syntax:max "sscanf!" scanString(term):term

-- open Lean Elab Command

-- syntax (name := xxx) "#red" : command
-- syntax (name := yyy) "#green" : command
-- syntax (name := zzz) "blue" : command

-- @[macro xxx] def redMacro : Macro := λ stx =>
--   match stx with
--   | _ => `(#green)

-- @[macro yyy] def greenMacro : Macro := λ stx =>
--   match stx with
--   | _ => `(blue)

-- @[command_elab zzz] def blueElab : CommandElab := λ stx =>
--   Lean.logInfo "finally, blue!"

-- #red 


-- def sscanf (s:String) (t:String) : 

def sscanf (s:String) (f:String) : Option List :=
  

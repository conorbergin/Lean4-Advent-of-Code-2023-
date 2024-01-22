import Lean

open Lean Elab

declare_syntax_cat parsecTerm
declare_syntax_cat parsecSpec

syntax (term)? (" : " parsecSpec)? : parsecTerm

syntax (name := parsecStr) "parsec!" interpolatedStr(parsecTerm) : term

syntax "r16" : parsecSpec

/-- Represents the data after the `:` in a `parsec!` string,
along with other incidental configuration. -/
structure ParsecSpec where
  /-- If not `none`, parse without including this character.
  This comes from the character immediately after the `}`. -/
  untilChar? : Option Char
  radix : Nat := 10

instance : Quote Char where
  quote c :=
    let digs := Nat.toDigits 16 c.toNat
    let digs := (List.replicate (4 - digs.length) '0') ++ digs
    let n := String.mk digs
    Syntax.mkLit charLitKind s!"'\\u{n}'"

instance : Quote ParsecSpec where
  quote spec := Unhygienic.run
    `(ParsecSpec.mk $(quote spec.untilChar?) $(quote spec.radix))

/-- Predicate for the character not being the one in `untilChar?`. -/
def ParsecSpec.notUntilChar (spec : ParsecSpec) (c : Char) : Bool :=
  spec.untilChar? |>.map (· != c) |>.getD true

/-- Class for associating parsers to types. -/
class ParsecParse (α : Type) where
  parse : ParsecSpec → Lean.Parsec α

instance : ParsecParse Nat where
  parse spec := do
    -- TODO: Should handle `spec.radix` in here.
    let chars ← Parsec.many1Chars (Parsec.satisfy fun c => c.isDigit && spec.notUntilChar c)
    return chars.toNat!

instance : ParsecParse Int where
  parse spec := do
    let chars ← Parsec.many1Chars (Parsec.satisfy fun c => (c.isDigit || c == '-') && spec.notUntilChar c )
    return chars.toInt!


instance : ParsecParse String where
  parse spec := Parsec.manyChars (Parsec.satisfy spec.notUntilChar)

/-- Modify the `spec` given the contents of `specStx?`. -/
def processParsecSpec (specStx? : Option (TSyntax `parsecSpec)) (spec : ParsecSpec) :
    MacroM ParsecSpec :=
  match specStx? with
  | some specStx =>
    match specStx with
    | `(parsecSpec| r16) => return {spec with radix := 16}
    | _ => Macro.throwUnsupported
  | none => return spec

def processParsecTerm (term : TSyntax `parsecTerm) (untilChar? : Option Char) :
    MacroM (Term × ParsecSpec) := do
  match term with
  | `(parsecTerm| $[$ty?]? $[: $spec?]?) =>
    let spec ← processParsecSpec spec? {untilChar?}
    let ty ← if let some ty := ty? then pure ty else `(_)
    return (ty, spec)
  | _ => Macro.throwUnsupported

def mkTuple (xs : Array Term) : MacroM Term :=
  match xs with
  | #[] => `(())
  | #[x] => return x
  | _ => `(($(xs[0]!), $(xs[1:]),*))

def expandChunks (chunks : Array Syntax) : MacroM Term := withFreshMacroScope do
  -- Array of binder/parser pairs
  let mut parsers : List (Option Term × Term) := []
  let mut res : List Term := []
  let mut untilChar? : Option Char := none
  for j in [0:chunks.size] do
    -- Reverse order for `untilChar?` handling
    let i := chunks.size - 1 - j
    let elem := chunks[i]!
    match elem.isInterpolatedStrLit? with
      | none =>
        let n := mkIdentFrom elem (← MonadQuotation.addMacroScope (Name.appendIndexAfter `n i))
        let (ty, spec) ← processParsecTerm ⟨elem⟩ untilChar?
        parsers := (n, ← `((ParsecParse.parse $(quote spec) : Parsec $ty))) :: parsers
        res := n :: res
        untilChar? := none
      | some str =>
        if str.utf8ByteSize > 0 then
          parsers := (none, ← `(Parsec.skipString $(quote str))) :: parsers
        untilChar? := str.get? ⟨0⟩
  let res' ← `(pure $(← mkTuple res.toArray))
  parsers.foldrM (init := res') fun (binder?, parser) t => do
    if let some binder := binder? then
      `($parser >>= fun $binder => $t)
    else
      `($parser *> $t)

macro_rules
  | `(parsec! $str) => do
    let t ← expandChunks str.raw.getArgs
    return t



#check parsec!"{String}: {Nat} - {Int}"




-- Testing
#eval (parsec!"{String}: {Nat} - {Nat} {Int}").run "entry1: 1 - 2 -3"

/-
Except.ok ("entry1", 1, 2)
-/

--parse!"{}: {.toNat?} [{.splitOn " " |>.filterMap String.toInt}] | {}" "john: 42 [-1 3 4 5 5]"

def parseNatIntString (s : String): Except String (Nat × Int × String) := do
  let n_1 ← Except.ok 2
  let n_2 ← Except.ok (-3)
  let n_3 := "Helo"
  return (n_1,n_2,n_3)


-- This works too:
#eval (parsec!"{}: {} + {}" : Parsec (String × Nat × Nat)).run "entry1: 1 + 2 / -3"

structure Color where
  (r g b : Nat)
  deriving Repr

instance : ParsecParse Color where
  parse _ := do
    let (r, g, b) ← parsec!"({},{},{})"
    return {r, g, b}

#eval (parsec!"color = {Color}").run "color = (1,2,3)"

syntax "parse!" interpolatedStr(term) term : term
macro_rules
  | `(parse!$f $s) => `((parsec!$f).run $s)

#eval if let Except.ok (a,b) := (parsec!"{String}-|-{String}").run "1 f-|-fkdl fd l df" then some (a,b) else none


-- parsec!"This {String.toNat?} is {String.splitOn " " |>.filterMap String.toNat?}"

#eval (parsec!"hello{String}").run "hellom"

#eval parse!"hello" "hellom"



partial def sItr (s:String) :=
  let rec loop (i : String.Pos) (n:Nat) :=
    let c := s.get! i
    if (s.next i) == s.endPos then
      c
    else
      loop (s.next i) (n+1)
  loop 0 0

#eval sItr "helo"


-- Something "hello @" "hello 100"
partial def scanner (s:String) (f:String) (fn: String → α) : Except String α :=
  -- let mut s := ""
  let rec loop (i₁: String.Pos) (i₂ : String.Pos) (r:String) (inside : Bool) : Except String α :=
    if i₁ == s.endPos ∨ i₂ == f.endPos then
      if i₁ != s.endPos then
        Except.error "Source too long"
      else if i₂ != f.endPos then
        Except.error "source too short"
      else
        Except.ok (fn r)
    else
      let c₁ := s.get! i₁
      let c₂ := f.get! i₂

      if inside then
        if c₁ == c₂ then
          loop (s.next i₁) (f.next i₂) r false
        else
          loop (s.next i₁) i₂ (r.push c₁) true
      else if c₁ == c₂ then
        loop (s.next i₁) (f.next i₂) r false
      else if c₂ == '@' then
          loop (s.next i₁ ) (f.next i₂) (r.push c₁) true
      else
        Except.error "Bad match"
  loop 0 0 "" false

#eval scanner "h1ello" "h@ello" String.toNat?



#eval (1).map (fun x => x+1)


-- Regex
declare_syntax_cat regex
syntax Sym := "a" <|> "b" <|> "c"
syntax Range := Sym "-" Sym
syntax Syms := Sym*
syntax Group := "(" + Sym + ")"
syntax RegEx := Sym <|> Group <|> Range

syntax "r!" RegEx : regex

#check r! "a-b"

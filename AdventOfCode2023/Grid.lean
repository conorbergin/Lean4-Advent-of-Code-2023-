import Lean
open Lean Macro

structure Grid (m : Nat) (n : Nat) (α : Type u) : Type u :=
  data : Array α
  correct: data.size = m * n




/-

-- Define a dependent type constructor for Grid
def mkGrid {α : Type u} (rows cols : Nat) (buf : Array A) (h : rows * cols = buf.size) : Grid A :=
  {rows := rows, cols := cols, buf := buf, e := h}

-- Define the ToString instance
instance {α : Type u} [ToString α] : ToString (Grid m n α) where
  toString g := Id.run do
    let mut r := ""
    for i in [:m] do
      for j in [:n] do
        r := r ++  g[i*m+j].toString
      r := r ++ "\n"
    r

-- Example usage
def exampleGrid : Grid Nat :=
  mkGrid 2 3 #[1, 2, 3, 4, 5, 6] rfl

#eval exampleGrid.toString


def Grid.get {A} (a : Grid A) (i : Fin a.rows) (j : Fin a.cols) : A :=
  a.buf.get ⟨i * a.cols + j, sorry⟩


def List.Grid {A} (cols : Nat) (l : List A) : Grid A :=
  let rows := l.length / cols
  let buf := (l.take (rows * cols)).toArray
  ⟨rows, cols, buf, sorry⟩

syntax "##[" sepBy(sepBy(term, ", "), "; ") "]" : term

macro_rules | `(##[ $[$elems,*];* ]) => do
  let rows := elems.size
  if rows = 1 then throwUnsupported
  let some a ← pure $ elems.get? 0 | throwUnsupported
  let cols := a.getElems.size
  let mut args := #[]
  for a in elems do
    let x := a.getElems
    unless x.size = cols do throwUnsupported
    args := args.append x
  `(List.Grid $(quote cols) [ $args,* ])

macro arr:term "[" i:term "," j:term "]" : term =>
  `(Grid.get $arr ⟨$i, by decide⟩ ⟨$j, by decide⟩)


macro arr:term "[" i:term "]" : term =>
  `(Grid.buf.get $arr ⟨$i, sorry⟩)


#eval ##[1,2,3,4;1,5,3,4]


#eval #[1,2,3,4,5].get (1: Fin 5)

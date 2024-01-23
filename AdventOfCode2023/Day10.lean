import AdventOfCode2023.Input.Day10


def minput :=
"..F7.
.FJ|.
SJ.L7
|F--J
LJ..."


#eval input.find (· == 'S')

#eval input.splitOn "\n" |>.toArray.map (·.toList.toArray)

partial def findLoop (m: Array (Array Char)) :=
  let rec loop (count: Nat) (i : Int) (j: Int) (di: Int) (dj: Int) :=
    if let some c := m[(i+di).toNat]?.bind (·[(j+dj).toNat]?) then
      match c with
      | 'F' => loop (count+1) (i+di) (j+dj) dj di
      | '7' => loop (count+1) (i+di) (j+dj) (-dj) di
      | 'J' => loop (count+1) (i+di) (j+dj) dj (-di)
      | 'L' => loop (count+1) (i+di) (j+dj) (-dj) (-di)
      | '-' => loop (count+1) (i+di) (j+dj) 0 dj
      | '|' => loop (count+1) (i+di) (j+dj) di 0
      | 'S' => Except.ok count
      | _ => Except.error s!"Invalid Location, count:{count}"
    else
      Except.error s!"bad index, count {count}"

  if let some i ← m.findIdxM? (λ a => ) then

      loop 0 i j  0 1

  else
    Except.error "no S"


def mm := #[1,2,23,4]

#eval  findLoop <| (minput.splitOn "\n").toArray.map (·.toList.toArray)

#eval choose [1,2,3]

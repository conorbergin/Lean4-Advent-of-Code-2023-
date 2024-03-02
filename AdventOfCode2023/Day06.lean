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

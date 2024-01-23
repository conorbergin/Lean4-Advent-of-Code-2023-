import AdventOfCode2023.Input.Day7


def minput := "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"

def parse (s:String) := 0

#eval [(2,3)] ++ [(2,4)]

def evalHand (h : List Nat):=
  h.foldl (λ (acc : List (Nat × Nat)) c =>
    if let some (_,n) := acc.find? (λ i  => i.fst == c) then
      acc.filter (·.fst != c) ++ [(c,n+1)]
    else acc ++ [(c,1)]) []
  |>.map (λ l => l.snd)
  |>.toArray.qsort (· ≥ ·)
  |>.toList


def parseCard (s : String) :=
  s.trim.toList
  |>.map (λ c => if let some n := c.toString.toNat? then n else
      match c with
      | 'T' => 10
      | 'J' => 11
      | 'Q' => 12
      | 'K' => 13
      | 'A' => 14
      | _ => 0 )

def rank (s₁ : String) (s₂ : String) : Bool :=
  let (h₁,h₂) := (parseCard s₁, parseCard s₂)
  let (e₁, e₂) := (evalHand h₁, evalHand h₂)
  if let some (a,b) := (List.zip e₁ e₂).find? (λ i: Nat × Nat => i.fst != i.snd) then
    a ≥ b
  else
    if let some (a,b) := (List.zip h₁ h₂).find? (λ i : Nat × Nat => i.fst != i.snd ) then
      a ≥ b
    else
      true

#eval parseCard  "12211"

#eval rank "21" "22"

#eval #[1,2,3,0]

#eval input.splitOn "\n"
  |>.filterMap (λ s => if let [a,b] := s.splitOn " " then some (a,b) else none) |>.toArray.qsort (λ (a:String×String) (b:String×String) => rank a.fst b.fst)|>.toList.enum.map (λ i => (i.fst +1)* i.snd.snd.toNat!) |>.foldl (λ acc i => acc + i) 0


#eval [1,2,3]

def input := include_str "input" / "day04.txt"



def minput := "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

#eval 1 != 1

def parseCard? (s:String) : Option (List Nat × List Nat):=
  if let [_, winning, numbers ] := s.split (λ c => c == ':' || c == '|') then
    let parseNumbers (s : String) : List Nat := s.trim.splitOn " " |>.filterMap String.toNat?
    (parseNumbers winning, parseNumbers numbers)
  else none

#eval parseCard? "jfdkj : 34 54 65 | 34 22 22 "

def countWinning (card: List Nat × List Nat) : Nat :=
  card.fst.filter (λ n => card.snd.any (· == n))
  |>.length

--  1 + 1 + 2 + 4 + 8
#eval countWinning ([41,48,83,86],[83,86,6,31,17,9,48,53])

-- Part 1
#eval input.splitOn "\n" |>.filterMap parseCard? |>.map countWinning |>.map (λ n => if n == 0 then 0 else 2^(n-1)) |>.foldl (λ n acc => acc + n) 0


-- Part 2

def input := include_str "input" / "day07.txt"



def minput := "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"


def minput2 := "2345A 1
Q2KJJ 13
Q2Q2Q 19
T3T3J 17
T3Q33 11
2345J 3
J345A 2
32T3K 5
T55J5 29
KK677 7
KTJJT 34
QQQJA 31
JJJJJ 37
JAAAA 43
AAAAJ 59
AAAAA 61
2AAAA 23
2JJJJ 53
JJJJ2 41"


-- maps a card to a Nat, Aces high
def parseHand (s : String) (jokers := false) :=
  s.trim.toList.map
    (λ c => if let some n := c.toString.toNat? then
      n
     else
      match c with
      | 'T' => 10
      | 'J' => if jokers then 1 else 11
      | 'Q' => 12
      | 'K' => 13
      | 'A' => 14
      | _ => 0 )

#eval parseHand "KKQQQ"


/-
  evaluates the hand:
  five of a kind -> 6
  four of a kind -> 5
  full house -> 4
  three of a kind -> 3
  two pair -> 2
  one pair -> 1 -/

def mergeDuplicates (h : List Nat):=
  h.foldl (λ (acc : List (Nat × Nat)) c =>
    if let some (_,n) := acc.find? (λ i  => i.fst == c) then
      acc.filter (·.fst != c) ++ [(c,n+1)]
    else acc ++ [(c,1)]) []
  |>.map (λ l => l.snd)
  |>.toArray.qsort (· > ·)
  |>.toList

#eval mergeDuplicates [8,8,8,3,8]

def mergeDuplicatesWithJokers (h: List Nat) :=
  let (n,j) := h.foldl (fun acc c => if c == 1 then (acc.fst,acc.snd + c) else (acc.fst ++ [c], acc.snd)) ([],0)
  let m := mergeDuplicates n
  if j == 5 then
    [5]
  else
    [j + m.head!] ++ m.tail!

#eval mergeDuplicatesWithJokers [2,2,2,2,2]


-- prepends the rank of the cards with he rank of the hand
def evalHand (s : String)(jokers := false):=
  let p := parseHand (jokers := jokers) s
  [(match (if jokers then mergeDuplicatesWithJokers p else mergeDuplicates p) with
  | [5] => 6
  | [4,1] => 5
  | [3,2] => 4
  | [3,1,1] => 3
  | [2,2,1] => 2
  | [2,1,1,1] => 1
  | _ => 0)] ++ p

#eval evalHand "KKKKK"
#eval evalHand "J345A"
#eval evalHand "32T3K"

#eval parseHand "32T3K" |> mergeDuplicates



def compareHands (l:List Nat) (r:List Nat) : Bool := Id.run do
  for t in List.zip l r do
    if t.fst == t.snd then
      continue
    else
      return t.fst < t.snd
  return true

#eval compareHands (evalHand "2345J") (evalHand "2345A")
#eval compareHands (evalHand "23456") (evalHand "23456")
#eval compareHands (evalHand "J345A") (evalHand "32T3K")



def countScore (s:String) (jokers := false) :=
  let data := s.splitOn "\n"
  |>.map (fun x => x.splitOn " " |> (fun l => (evalHand (jokers:=jokers) l[0]!,l[1]!.toNat!)))
  |>.toArray

  let sorted := data.qsort (compareHands ·.fst ·.fst)

  sorted.map (·.snd)
  |>.toList.enum.map (fun (a,b) => (a+1,b))
  |>.foldl (fun acc (a,b) => a*b + acc) 0


-- part 1
#eval countScore  input

--part 2
#eval countScore (jokers := true) minput2

def input := include_str "Input" / "day01.txt"

def minput := "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"

def minput2 := "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"


-- Part 1

def buildNat? : List Nat → Option Nat
  | [] => none
  | xs => some (xs[0]!*10 + xs.getLast!)

#eval buildNat? [1]



#eval input.splitOn "\n" |>.map (·.toList.filterMap (·.toString.toNat?)) |>.filterMap buildNat? |>.foldl Add.add 0


-- Part 2

def numberMap : List ((List String) × Nat) := [
  (["one",  "1"],1),
  (["two",  "2"],2),
  (["three","3"],3),
  (["four", "4"],4),
  (["five", "5"],5),
  (["six",  "6"],6),
  (["seven","7"],7),
  (["eight","8"],8),
  (["nine", "9"],9)
]

#eval String.substrEq "hello" ⟨0⟩ "hello" ⟨0⟩ 4

partial def parseNat2 (s: String) :=
  let rec loop (i: String.Pos) (l: List Nat) :=
    let ln := numberMap.find? (λ t => t.fst.any (λ v => String.substrEq v ⟨0⟩ s i v.length ))
    let l := match ln with
      | some n => l ++ [n.snd]
      | none => l
    if s.atEnd i then
      l
    else
      loop (s.next i) l
  loop ⟨0⟩ []



#eval input.splitOn "\n" |>.map parseNat2 |>.filterMap buildNat? |>.foldl Add.add 0

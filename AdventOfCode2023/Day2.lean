import AdventOfCode2023.Input.Day2


def minput :=
"Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

inductive Color where
  | red
  | blue
  | green
  deriving BEq, Repr

def stringToColor? : String → Option Color
  | "red" => some Color.red
  | "blue" => some Color.blue
  | "green" => some Color.green
  | _ => none

def parsePick? (s : String) : Option (Nat × Color) :=
  if let [num, color] := (s.trim.splitOn " ") then
    if let (some n, some c) := (num.toNat?, stringToColor? color) then some (n, c) else none
  else none

#eval parsePick? "1 blue"

def parseGame (s:String) :=
  (s.splitOn ";").map (λ a => (a.splitOn ",").filterMap parsePick?)
  |>.filter (·.length > 0)

#eval parseGame "1 red, 2 blue, 3 green; 4 red, 2 blue"

def parse (inp:String) :=
  inp.splitOn "\n"
  |>.map (λ line => (line.splitOn ":").getLast!.trim |> parseGame)

#eval parse minput


def isGamePossible (game : List (List (Nat × Color))) (red := 0) (green := 0) (blue := 0) :=
  let config : Color → Nat
  | Color.red => red
  | Color.green => green
  | Color.blue => blue
  game.all (λ g => g.all (λ p => p.fst ≤ config p.snd ))

#eval parse input |>.enum |>.filter (·.snd |> isGamePossible (red := 12) (green := 13) (blue := 14)) |>.foldl (λ acc x => acc + x.fst + 1) 0

def gamePower (game: List (List (Nat × Color)))  :=
  let getColor? (g : List (Nat × Color)) (c:Color) : Option Nat := if let some p := g.find? (·.snd == c) then some p.fst else none
  let maxColor? (g : List (List (Nat × Color))) (c:Color) : Nat  := ((g.filterMap (λ p => getColor? p c) : List Nat).foldl (λ m c => if c > m then c else m) 0)
  let maxColors : List Nat := [Color.red, Color.green, Color.blue].map (λ c => maxColor? game c)
  maxColors.foldl (λ a b => a*b) 1

#eval parse input |>.map gamePower |>.foldl (λ a b => a + b) 0

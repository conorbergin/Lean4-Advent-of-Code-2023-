def input := include_str "input" / "day09.txt"



def minput := "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45"


-- Part 1

def parse := 0

def getNextValue (l: List Int):=
  let rec loop (acc: List (List Int)) :=
    let u := acc.getLast!
    let d := List.zip u.tail! (u.take (u.length - 1)) |>.map (λ (a,b) => a - b)
    if d.all (· == (0:Int)) then
      some (acc.concat d)
    else if d.length == 1 then
      none
    else
      loop (acc.concat d)
  if let some r := loop [l] then
    some (r.foldl (λ acc l => (List.getLast! l) + acc) (0:Int))
  else
    none
decreasing_by sorry

#eval getNextValue [0, 3, 6, 9, 12, 15]

#eval input.splitOn "\n" |>.map (λ l => l.splitOn " " |>.filterMap String.toInt? ) |>.filterMap getNextValue |>.foldl Add.add 0


-- Part 2
partial def getPrevValue (l: List Int):=
  let rec loop (acc: List (List Int)) :=
    let u := acc.getLast!
    let d := List.zip u.tail! (u.take (u.length - 1)) |>.map (λ (a,b) => a - b)
    if d.all (· == (0:Int)) then
      some (acc.concat d)
    else if d.length == 1 then
      none
    else
      loop (acc.concat d)
  if let some r := loop [l] then
    some (r.foldl (λ acc l => (List.head! l) - acc) (0:Int))
  else
    none

#eval getPrevValue [10, 13, 16, 21, 30, 45]

#eval minput.splitOn "\n" |>.map (λ l => l.splitOn " " |>.filterMap String.toInt? ) |>.filterMap getPrevValue |>.foldl Add.add 0

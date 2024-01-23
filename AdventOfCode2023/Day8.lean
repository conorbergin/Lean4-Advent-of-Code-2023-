import AdventOfCode2023.Input.Day8


def minput := "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)"


def parse? (s: String) :=
  if let [path,network] := s.splitOn "\n\n" then
    let n := network.splitOn "\n"
      |>.map ( String.replace  "(" "")
      |>.map  (String.replace ")" "")
      |>.filterMap (fun l => if let [a,b,c] := String.splitOn l " " then some (a,(b,c)) else none)
    some (path.toList,n)
  else
    none

#eval parse? minput

def input := include_str "input" / "day03.txt"

def minput := "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."

def minput2 :=
"12.......*..
+.........34
.......-12..
..78........
..*....60...
78.........9
.5.....23..$
8...90*12...
............
2.2......12.
.*.........*
1.1..503+.56"

-- Part 1


def getSurrounding (i j length:Nat) := Id.run do
  let mut result := []
  for ii in [i-1:i+2] do
    for jj in [j-1:j+1+length] do
      result := result.concat (ii,jj)
  result

#eval getSurrounding 1 1 1

def solveBoth (str :String) := Id.run do
  let mut symIndex := []
  let mut gearIndex := []
  let mut numIndex : List (String×(Nat×Nat)) := []


  for (i,line) in str.splitOn "\n" |>.enum do

    let mut state := none

    for (j,c) in line.toList.enum do
      if "0123456789".contains c then

        if let some s := state then
          state := some (s.push c)
        else
          state := some c.toString
      else
        if let some s := state then
          numIndex := numIndex.concat (s,(i,j-s.length))
        state := none
        if c != '.' then
          symIndex := symIndex.concat (i,j)
          if c == '*' then
            gearIndex := gearIndex.concat (i,j)

    if let some s := state then
      numIndex := numIndex.concat (s,(i,line.length-s.length))


  let part1 :=
    numIndex
    |>.filter (λ (n,(i,j)) =>
      getSurrounding i j (n.length)
      |>.any (symIndex.contains ·))
    |>.foldl (λ acc (n,_) => acc + n.toNat!) 0

  let part2 := Id.run do
    let mut res := 0
    for g in gearIndex do
      let n := numIndex.filter (λ (n,(i,j)) =>
        getSurrounding i j (n.length)
        |>.any (g == ·))
      if n.length == 2 then
        res := res + (n[0]!.fst.toNat! * n[1]!.fst.toNat!)
    return res



  return (part1,part2)


#eval solveBoth input
#eval 530849 - 528369

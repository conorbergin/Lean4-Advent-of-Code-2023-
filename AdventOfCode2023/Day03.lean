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

-- Part 1

inductive Token :Type
| Number : Nat -> Token
| Space : Token
| Symbol : Token
deriving Repr, BEq

def getToken (c:Char) :=
  if let some n := c.toString.toNat? then
    Token.Number n
  else if c == '.' then
    Token.Space
  else Token.Symbol


-- #eval List.filter
-- hello
def part1  (s:String) := Id.run do
  let mut sum := []

  let rows := s.splitOn "\n"

  for (i,row) in rows.enum do

    let mut currentNumber : Option (Nat) := none

    let prevRow := if i == 0 then none else some (rows.get! (i - 1))
    let nextRow := if i == rows.length - 1 then none else some (rows.get! (i+1))

    for (j,item) in row.toList.enum do

      let token := getToken item
      -- read number if exists
      if let Token.Number n := token then
        if let some m := currentNumber then
          currentNumber := some (m*10 + n)
        else
          currentNumber := some n



  sum

#eval part1 minput


#eval [1,2,3].map (fun x ↦ x+1)

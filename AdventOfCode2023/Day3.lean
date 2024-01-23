import AdventOfCode2023.Input.Day3


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
    let mut prevHasSymbol := false

    let prev := if i == 0 then none else some (rows.get! (i - 1))
    let next := if i == rows.length - 1 then none else some (rows.get! (i+1))

    for (j,item) in row.toList.enum do

      let token := getToken item

      -- checks for a symbol on the current column
      let hasSymbol := Id.run do
        let mut col := [token]
        if let some l := prev then
          col := col ++  [getToken (l.get! ⟨j-1⟩)]
        if let some l := next then
          col := col ++  [getToken (l.get! ⟨j-1⟩)]
        col.any (· == Token.Symbol)

      --

      if let some (n) := currentNumber then
        if let Token.Number m := token then
          --append to number
          currentNumber := some (n*10 + m)
          -- last col
          if j == row.length - 1 then
            sum := sum ++ [currentNumber.get!]
            prevHasSymbol := false
          else ()
        else if hasSymbol || prevHasSymbol then
          sum := sum ++ [n]
        else ()
      else if let Token.Number n := token then
        currentNumber := some n

      else if !hasSymbol then ()
        -- prevHasSymbol := false

  sum

#eval part1 minput

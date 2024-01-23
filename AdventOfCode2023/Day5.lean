import AdventOfCode2023.Input.Day5


def minput := "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"

def hello := "hello"

def parse (s: String) :=
  let parseInput (s:String) := s.trim.splitOn " " |>.filterMap String.toInt?
  let parseTriple? (s:String) := if let [a,b,c] := s.trim.splitOn " " |>.filterMap String.toInt? then some (a,b,c) else none
  let parseMap (s: String) := s.splitOn "\n" |>.filterMap parseTriple?
  let a := s.trim.splitOn "\n\n"
  let inp := parseInput a.head!
  let maps := a.tail! |>.map  parseMap
  (inp, maps)


def solveFn (inp : List Int) (maps : List (List (Int × Int × Int))) :=
  let applyMap (inp : List Int) (m: List (Int × Int × Int)) :=
    inp.map (λ x =>
      if let some (d,s,l) := m.find? (λ (d,s,l) => x ≥ s ∧ x ≤ (s + l)) then
        (x + d) - s
      else
        x)

  maps.foldl (λ acc n => applyMap acc n ) inp

#eval parse input |> λ (i,m) => solveFn i m |>.foldl (λ acc i => if i < acc then i else acc) 10000000000000

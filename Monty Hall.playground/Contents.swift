enum Coin: String   { case H, T }
enum Result: String { case Win, Lose }

func play(c: Coin) -> Prob<Result> {
  switch c {
  case .H: return Prob([(.Win,0.7),(.Lose,0.3)])
  case .T: return [.Win,.Lose].equalProbs
  }
}

func comp(a: Result, b:Result) -> Ordering {
  switch (a,b) {
  case (.Win,.Win),(.Lose,.Lose): return .EQ
  case (.Lose,.Win): return .LT
  case (.Win,.Lose): return .GT
  }
}

[Coin.H,.T]
  .equalProbs
  .flatMap(play)
  .mergeProbs(comp)
  .description

Prob([(Coin.H,0.7),(.T,0.3)])
  .flatMap(play)
  .mergeProbs(comp)
  .description

public enum Choice { case Switch, Stick }

public func chances(n: Int, _ p: Int, _ c: Choice)(_ d: Int) -> Prob<Bool> {
  switch c {
  case .Stick : return (1...n).equalProbs.fmap(==d)
  case .Switch:
    let notFirst = chances(n,p,.Stick)(d).fmap(!)
    let second   = Repeat(count: n-p-2, repeatedValue: false) + [true]
    return notFirst.flatMap { f in second.equalProbs.fmap { s in f && s } }
  }
}

public func chanceOfCar(n: Int, _ p: Int, _ s: Choice) -> Prob<Bool> {
  return (1...n)
    .equalProbs
    .flatMap(chances(n,p,s))
    .mergeProbs(comp)
}

let a = chanceOfCar(3, 1, .Stick).description


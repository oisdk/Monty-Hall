struct Prob<Element> {
  private let contents: [(Element, Double)]
}

extension Prob : Indexable, CollectionType {
  var startIndex: Int { return contents.startIndex }
  var endIndex: Int { return contents.startIndex }
  subscript(i: Int) -> (Element, Double) {
    return contents[i]
  }
}

extension Prob {
  func fmap<T>(f: Element -> T) -> Prob<T> {
    return Prob<T>(contents: contents.map { (v,p) in (f(v), p) })
  }
  func flatMap<T>(f: Element -> Prob<T>) -> Prob<T> {
    let c = contents.flatMap { (v,p) in
      f(v).contents.map { (x,ip) in (x,p*ip) }
    }
    return Prob<T>(contents: c)
  }
}

extension CollectionType where Index == Int {
  var equalProbs: Prob<Generator.Element> {
    let p = 1.0 / Double(endIndex)
    return Prob<Generator.Element>(contents: map { v in (v,p) })
  }
}

enum Ordering { case LT, EQ, GT }

extension SequenceType {
  func mergeBy(
    comp: (Generator.Element, Generator.Element) -> Ordering,
    merg: (Generator.Element, Generator.Element) -> Generator.Element
    ) -> [Generator.Element] {
      var result: [Generator.Element] = []
      var curr: Generator.Element? = nil
      for h in sort({ e in comp(e) == .LT }) {
        guard let y = curr else { curr = h; continue }
        if case .EQ = comp(h,y) {
          curr = merg(h,y)
        } else {
          result.append(y)
          curr = h
        }
      }
      return result + (curr.map { e in [e] } ?? [])
  }
}

func comp<C : Comparable>(lhs: C, rhs: C) -> Ordering {
  if lhs < rhs { return .LT }
  if lhs > rhs { return .GT }
  return .EQ
}

extension Prob {
  func mergeProbs(comp: (Element,Element) -> Ordering) -> Prob {
    return Prob(contents:
      contents.mergeBy({(a,b) in comp(a.0,b.0)}, merg: {(a,b) in (a.0,a.1+b.1)})
    )
  }
}

enum Door { case L, M, R }
enum Choice { case Switch, Stick }

func chances(d: Door, c: Choice) -> Prob<Bool> {
  switch c {
  case .Stick : return [Door.L,.M,.R].equalProbs.fmap { o in o == d }
  case .Switch: return [Door.L,.M,.R].equalProbs.fmap { o in o != d }
  }
}
public func <(lhs: Bool, rhs: Bool) -> Bool {
  if lhs { return false }
  return rhs
}
extension Bool : Comparable {}

func chanceOfCar(s: Choice) -> Prob<Bool> {
  return [Door.L,.M,.R]
    .map { d in (d,s) }
    .equalProbs
    .flatMap(chances)
    .mergeProbs(comp)
}

extension Prob : CustomStringConvertible {
  var description: String {
    return contents.map { (v,p) in
      String(p) + ": " + String(reflecting: v)
    }.joinWithSeparator("\n")
  }
}

chanceOfCar(.Switch).description

public struct Prob<Element> {
  private let contents: [(Element, Double)]
}
extension Prob {
  public init(_ p: [(Element,Double)]) { contents = p }
}

extension Prob {
  public func fmap<T>(f: Element -> T) -> Prob<T> {
    return Prob<T>(contents.map { (v,p) in (f(v), p) })
  }
}

extension Prob {
  public func flatMap<T>(f: Element -> Prob<T>) -> Prob<T> {
    return Prob<T>(contents.flatMap { (v,p) in
      f(v).contents.map { (x,ip) in (x,p*ip) }
    })
  }
}

extension CollectionType where Index.Distance == Int {
  public var equalProbs: Prob<Generator.Element> {
    let p = 1 / Double(count)
    return Prob<Generator.Element>(map { v in (v,p) })
  }
}

public enum Ordering { case LT, EQ, GT }

extension SequenceType {
  private typealias A = Generator.Element
  public func mergeBy(comp: (A, A) -> Ordering, _ merge: (A, A) -> A) -> [A] {
    var result: [A] = []
    for h in sort({ (a,b) in comp(a,b) == .LT }) {
      if case .EQ? = result.last.map({e in comp(h,e)}) {
        let y = result.removeLast()
        result.append(merge(y,h))
      } else {
        result.append(h)
      }
    }
    return result
  }
}

extension Prob {
  public func mergeProbs(comp: (Element,Element) -> Ordering) -> Prob {
    return Prob(contents:
      contents.mergeBy({(a,b) in comp(a.0,b.0)}, {(a,b) in (a.0,a.1+b.1)})
    )
  }
}
prefix operator == {}
public prefix func ==<E : Equatable>(rhs: E)(_ lhs: E) -> Bool {
  return lhs == rhs
}
prefix operator != {}
public prefix func !=<E : Equatable>(rhs: E)(_ lhs: E) -> Bool {
  return lhs != rhs
}

public func comp(lhs: Bool, rhs: Bool) -> Ordering {
  switch (lhs,rhs) {
  case (true,true),(false,false): return .EQ
  case (false,true): return .LT
  case (true,false): return .GT
  }
}

extension Prob : CustomStringConvertible {
  public var description: String {
    return contents.map { (v,p) in
      String(p) + ": " + String(reflecting: v)
      }.joinWithSeparator("\n")
  }
}

extension Prob {
  public func and<T>(w: Prob<T>) -> Prob<(Element, T)> {
    return flatMap { a in
      w.fmap { b in (a,b) }
    }
  }
}

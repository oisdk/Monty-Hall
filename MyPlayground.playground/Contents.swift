protocol Monoid {
  static func + (_: Self, _: Self) -> Self
  static func mempty() -> Self
}

extension Array : Monoid {
  static func mempty() -> [Element] { return [] }
}

protocol FloatArithType : FloatingPointType, FloatLiteralConvertible, Monoid {
  static func + (_: Self, _: Self) -> Self
  static func - (_: Self, _: Self) -> Self
  static func * (_: Self, _: Self) -> Self
  static func / (_: Self, _: Self) -> Self
}

func merge<S : SequenceType, K : Hashable, M : Monoid where S.Generator.Element == (K,M)>(s: S) -> [K:M] {
  var result: [K:M] = [:]
  for (k,v) in s {
    result[k] = (result[k] ?? M.mempty()) + v
  }
  return result
}

func + <K : Hashable, M : Monoid>(var a: [K:M], b: [K:M]) -> [K:M] {
  for (k,v) in b {
    a[k] = (a[k] ?? M.mempty()) + v
  }
  return a
}

func merge<S : SequenceType, K0 : Hashable, K1 : Hashable, F : FloatArithType where S.Generator.Element == (K0,[K1:F])>(s: S) -> [K0:[K1:F]]{
  var result: [K0:[K1:F]] = [:]
  for (k,v) in s {
    result[k] = (result[k] ?? [:]) + v
  }
  return result
}

extension Double : FloatArithType {
  static func mempty() -> Double { return 0.0 }
}
extension Float  : FloatArithType {
  static func mempty() -> Float { return 0.0 }
}

extension Dictionary where Value : FloatArithType {
  static func pure(e: Key) -> [Key:Value] {
    return [e:1.0]
  }
  func fmap<K : Hashable>(f: Key -> K) -> [K:Value] {
    return merge(lazy.map { (k,v) in (f(k),v) })
  }
  func flatMap<
    S : SequenceType, K : Hashable
    where S.Generator.Element == (K,Value)
    >(transform: Key -> S) -> [K:Value] {
    return probFlatten(lazy.map { (v,p) in (transform(v),p) })
  }
}

extension Array where Element : Hashable {
  var equalProbs: [Element:Double] {
    let prob = 1 / Double(count)
    return merge(lazy.map { e in (e,prob) })
  }
}

func probFlatten<
  S0 : SequenceType, S1 : SequenceType,
  K : Hashable, N : FloatArithType where
  S0.Generator.Element == (S1,N),
  S1.Generator.Element == (K,N)
  >(s: S0) -> [K:N] {
  return merge(
    s.flatMap { (iS,p) in
      iS.map { (v,iP) in  (v,p*iP)   }
    }
  )
}

enum Door { case L, M, R }
enum Choice { case Stick, Switch }

let carIsBehind: [Door] = [.L,.M,.R]

func didGetCar(chose: Door, didSwitch: Choice) -> [Bool:Double] {
  return carIsBehind.equalProbs.fmap { c -> Bool in
    switch didSwitch {
    case .Stick : return c == chose
    case .Switch: return c != chose
    }
  }
}



let chosen: [Door] = [.L,.M,.R]
let chances = chosen
  .flatMap { d in [(d,Choice.Stick),(d,Choice.Switch)] }
  .map { (d,c) in (c,didGetCar(d, didSwitch: c)) }



print(merge(chances))
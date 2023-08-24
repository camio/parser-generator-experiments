extension Grammar {
  enum ParseMove: Hashable {
    case terminal(Symbol, at: Position)
    case begin(Symbol, at: Position)
    case end(Symbol)
  }

  typealias Parse = [ParseMove]


  func generateParses(
    maxLength: Int, maxDepth: Int, start: Symbol, into receiver: (Parse)->()
  ) {
    let rulesByLHS = MultiMap(grouping: rules, by: \.lhs)
    var parse = Parse()
    var length = 0
    var depth = 0

    generateNonterminal(start, at: -1) { receiver(parse) }

    func generateTerminal(_ s: Symbol, at p: Position, then onward: ()->()) {
      if length == maxLength { return }
      length += 1
      parse.append(.terminal(s, at: p))
      onward()
    }

    func generateNonterminal(_ s: Symbol, at p: Position, then onward: ()->()) {
      if depth == maxDepth { return }
      depth += 1
      parse.append(.begin(s, at: p))
      let mark = (length: length, depth: depth, parseCount: parse.count)
      for r in rulesByLHS[s] {
        parse.removeSubrange(mark.parseCount...)
        (length, depth) = (mark.length, mark.depth)
        generateString(r.rhs) { parse.append(.end(s)); onward() }
      }
    }

    func generateSymbol(at p: Position, then onward: ()->()) {
      let s = postdot(at: p)!
      if rulesByLHS.storage[s] != nil {
        generateNonterminal(s, at: p, then: onward)
      }
      else {
        generateTerminal(s, at: p, then: onward)
      }
    }

    func generateString(_ s: Array<Symbol>.SubSequence, then onward: ()->()) {
      if s.isEmpty { return onward() }
      let depthMark = depth
      generateSymbol(at: .init(s.startIndex)) {
        depth = depthMark
        generateString(s.dropFirst(), then: onward)
      }
    }
  }
}

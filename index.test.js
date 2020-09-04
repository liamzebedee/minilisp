const dumblisp = require('./index');
const {
  builtins
} = dumblisp

const testcases = [
  // {
  //   input: "(1 2)",
  //   output: "[ 1, 2 ]"
  // },
  {
    input: `(quote (1 a 3))`,
    output: `[ 1, 'a', 3 ]`
  },
  {
    input: `(eq 1 1)`,
    output: `true`
  },
  {
    input: `(eq 1 2)`,
    output: `[]`
  },
  {
    input: `(car (quote (1 2)))`,
    output: "1"
  },
  {
    input: `(car (cdr (quote (1 2 3))))`,
    output: "2"
  },
  {
    input: `(car (cdr (quote (1 2 3))))`,
    output: "2"
  },
  {
    input: `((lambda (f) (f (quote (b c))))
             (quote (lambda (x) (cons (quote a) x))))`,
    output: `[ 'a', 'b', 'c' ]`
  }
]

test('read', () => {
  expect(builtins.read("(1 2)")).toEqual([[1, 2]])
})

describe("run testcases", () => {
  testcases.map((testcase, i) => {
    test(testcase.input, () => {
      expect(dumblisp.run(testcase.input)).toEqual(testcase.output)
    })
  })
})

test('car', () => {
  expect(builtins.car([1, 2, 3])).toEqual(1)
})

test('cdr', () => {
  expect(builtins.cdr([1, 2, 3])).toEqual([2, 3])
})

test('atom', () => {
  expect(builtins.atom(1)).toBe(true)
  expect(builtins.atom("1")).toBe(true)
  expect(builtins.atom(false)).toBe(true)
  expect(builtins.atom([])).toBe(true)
  expect(builtins.atom([1, 2, 3])).toEqual([])
})

test('evcon', () => {
  const env = []
  expect(builtins.evcon([ [false, 2], [true, 1] ], env)).toEqual(1)
})

test('evlist', () => {
  const env = dumblisp.env
  expect(
    dumblisp.builtins.evlist([1, 2, 3], env)
  ).toEqual([1,2,3])
  expect(
    dumblisp.builtins.evlist([2], env)
  ).toEqual([2])
})

describe('assoc', () => {
  const env = dumblisp.env

  test('numbers and bools', () => {
    expect(builtins.assoc(1, env)).toEqual(1)
    expect(builtins.assoc(true, env)).toEqual(true)

    expect(builtins.assoc('t', env)).toEqual(true)
    expect(builtins.assoc('f', env)).toEqual(false)
  })
  test('symbols', () => {
    const envExtended = [
      ['n', 1],
      ...env
    ]
    expect(builtins.assoc('n', envExtended)).toEqual(1)
  })
  test('core functions', () => {
    expect(builtins.assoc('cons', env)).toEqual('cons')
  })
})

test('null', () => {
  expect(builtins.null_([])).toEqual(true)
})

test('evaluate', () => {
  expect(dumblisp.evaluate(['eq', 1, 2])).toEqual([])
  expect(dumblisp.evaluate(['eq', 1, 1])).toEqual(true)
  expect(dumblisp.evaluate(['quote', [1, 2, 3]])).toEqual([1, 2, 3])
  expect(dumblisp.evaluate(['quote', '123'])).toEqual('123')
  expect(dumblisp.evaluate(['quote', 'a'])).toEqual('a')
  expect(dumblisp.evaluate(['car', ['quote', [1, 2]]])).toEqual(1)

  // expect(() => dumblisp.evaluate([1, 2])).toThrow("unbound symbol: 1")
  expect(dumblisp.evaluate([1, 2])).toEqual(1)
  expect(() => dumblisp.evaluate(['eq', 1, 2, 3])).toThrow("eq expects 2 arguments, 3 given")
  expect(() => dumblisp.evaluate(['atom', 1, 3])).toThrow("atom expects 1 argument, 2 given")
})

test('cond', () => {
  // expect(dumblisp.run('((lambda (x) (cond ((= x 1) 10) (t 0))) (1))')).toEqual("10")
  expect(dumblisp.run('((lambda (x) (cond ((= x 1) 10) (t 0))) (5))')).toEqual("0")

})

describe('lambdas', () => {
  test('lambda function call', () => {
    expect(dumblisp.run('((lambda (x) (cons x (quote (b)))) (quote a))')).toEqual("[ 'a', 'b' ]")
    expect(dumblisp.run('((lambda (x) (= x 1)) (2))')).toEqual("[]")
    expect(dumblisp.run('((lambda (x) (= x 1)) (1))')).toEqual("true")
  })

  test('lambdas as arguments', () => {
    const code = `((lambda (f) (f (quote (b c))))
                    (quote (lambda (x) (cons (quote a) x))))`
    expect(dumblisp.run(code)).toEqual("[ 'a', 'b', 'c' ]")
  })
})

test('arithmetic', () => {
  expect(dumblisp.evaluate(['+', 1, 1])).toEqual(2)
  expect(dumblisp.evaluate(['-', 1, 1])).toEqual(0)
  expect(dumblisp.evaluate(['-', 1, 2])).toEqual(-1)
  expect(dumblisp.evaluate(['-', 0, ['+', 1, 1]])).toEqual(-2)
  expect(dumblisp.evaluate(['+', 0, ['+', 1, 1]])).toEqual(2)
})

describe('label', () => {
  test('range', () => {
    const codeBroken = 
`
((label range ((lambda (n) 
    (cond 
        ((= n 0) nil) 
        (t (cons n (range (- n 1))))))) (range 3))
`

    const code =
`
((label range (lambda (n) 
    (cond 
        ((= n 0) nil) 
        (t (cons n (range (- n 1))))))) 100)
`
    expect(dumblisp.evalLabel(dumblisp.read(code)[0], dumblisp.env)).toEqual([5, 4, 3, 2, 1])
  })
})
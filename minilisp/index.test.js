const dumblisp = require('./index');
const {
  eval_,
  builtins
} = dumblisp

const testcases = [
  // {
  //   input: "(1 2)",
  //   output: "[ 1, 2 ]"
  // },
  {
    input: `(quote (1 2 3))`,
    output: `[ 1, 2, 3 ]`
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
    input: `(car (1 2))`,
    output: "1"
  },
  {
    input: `(car (cdr (1 2 3)))`,
    output: "2"
  },
  {
    input: `(car (cdr (1 2 3)))`,
    output: "2"
  },
  // {
  //   input: `(pair '(x y z) '(a b c))`,
  //   output: `[[x, a], [y, b], [z, c]]`
  // }
  // {
  //   input: `
  //   (quote 12)
  //   `,
  //   output: "[ 12 ]"
  // },
]

test('read', () => {
  expect(builtins.read("(1 2)")).toEqual([[1, 2]])
})

describe("Evaluation testcases", () => {
  testcases.map((testcase, i) => {
    test(testcase.input, () => {
      expect(eval_(testcase.input)).toEqual(testcase.output)
    })
  })
})

// test('pair', () => {
//   expect(builtins.pair([1,2,3], [4,5,6])).toEqual([])
// })

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
  expect(builtins.evcon([ [false, 2], [true, 1] ])).toEqual(1)
})

test('evaluate', () => {
  expect(builtins.eval(['eq', 1, 2])).toEqual([])
  expect(builtins.eval(['eq', 1, 1])).toEqual(true)
  expect(builtins.eval(['quote', [1, 2, 3]])).toEqual([1, 2, 3])
  expect(builtins.eval(['quote', '123'])).toEqual('123')
})

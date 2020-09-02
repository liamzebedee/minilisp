const {inspect} = require('util');



// 
// LISP.
// 
const NIL = []

const T_NUMBER = Symbol('number')
const T_SYMBOL = Symbol('symbol')
const T_BOOL = Symbol('bool')

function lisp_type(x) {
    switch(typeof x) {
        case 'number': return T_NUMBER
        case 'string': return T_SYMBOL
        case 'boolean': return T_BOOL
        default: return NIL
    }
}

/**
 * Lists and pairs: (1 () (2 . 3) (4))
 * Symbols: with-hyphen ?@!$ a\ symbol\ with\ spaces
 * Strings: "Hello, world!"
 * Integers: -9876543210
 * Floating-point numbers: -0.0 6.28318 6.023e23
 */

const DBL_QUOTE_CHAR = '"'
const SEXPR_START = "("
const SEXPR_END = ")"


function read(source, from = 0, d = 0) {
    // An S-Expression is a very simple form.
    // SEXPR = "(" SEXPR | ATOM ")"
    // ATOM = STRING | INTEGER | FLOAT | SYMBOL
    // STRING = '"' * '"'
    // INTEGER = [-][\d]
    // FLOAT = [-][\d+][.][\d+]
    // SYMBOL = [a-zA-Z\-0-9]
    let acc = ""
    let items = []
    
    for(let i = from; i < source.length; i++) {
        let char = source[i]

        // S-Expressions.
        if(char == SEXPR_START) {
            const res = read(source, i + 1, d + 1)
            items.push(res.items)
            i = res.i
            continue
        }

        if(char == SEXPR_END) {
            return { items, i }
        }

        // Atoms.
        // 

        // STRING.
        if(char == DBL_QUOTE_CHAR) {
            // Find the next quote char.
            const endIdx = source.indexOf(DBL_QUOTE_CHAR, i + 1)
            if(endIdx == -1) {
                throw new Error(`error parsing string, expected terminating quote`)
            }
            // Get the raw string, ignoring the first quote char.
            const atom = source.slice(i + 1, endIdx)
            items.push(atom)
            i = endIdx
            continue
        }

        // INTEGER/FLOAT
        const INTEGER_FLOAT_REGEX = /^[\+\-]?\d*\.?\d+(?:[Ee][\+\-]?\d+)?/
        const integerFloatMatches = char.match(INTEGER_FLOAT_REGEX)
        if(integerFloatMatches) {
            // Now we seek to match the whole number pattern.
            const match = source.slice(i).match(INTEGER_FLOAT_REGEX)
            const atom = match[0]

            // TODO: hack.
            let num
            if(
                atom.indexOf('.') == -1 || 
                atom.indexOf('-') == -1
            ) {
                num = parseFloat(atom)
            } else num = parseInt(num)

            items.push(
                num
            )
            i = i + atom.length-1
            continue
        }

        // SYMBOL
        // const SYMBOL_REGEX = /^[a-zA-Z]{0}[^\(\)\s]+/
        const SYMBOL_REGEX = /^[^\(\)\s]{1,}/
        const symbolMatches = char.match(SYMBOL_REGEX)
        if(symbolMatches) {
            const match = source.slice(i).match(SYMBOL_REGEX)
            const atom = match[0].toLowerCase()
            items.push(atom)
            i = i + atom.length-1
            continue
        }

        if(char.match(/\s\r/)) continue
        if(char == "") continue
    }

    if(d != 0) throw new Error("error parsing s-expression: missing end paren")

    return items
}



function atom(x) {
    if(
        typeof x == 'number' || 
        typeof x == 'string' ||
        typeof x == 'boolean' || 
        x == NIL) 
    return true

    if(typeof x == 'object' && x.constructor.name == 'Array') {
        // The empty list is atomic.
        if(x.length === 0) return true
        // Else return an empty list.
        else return []
    }
}

function eq(x, y) {
    // deep eq
    if(atom(x) === true && atom(y) === true) {
        if(x === y) return true
    }

    if(x.length === 0 && y.length === 0) return true
    // if(x === [] && y === []) return true

    return []
}

function car(x) {
    if(x.constructor.name !== 'Array') throw new Error("car expects a list")
    if(x.length === 0) return NIL
    return x[0]
}

/** Pops the head and returns the rest of the list. */
function cdr(x) {
    if(x.constructor.name !== 'Array') throw new Error("cdr expects a list")
    return x.slice(1)
}

function cons(x, y) {
    return [x, ...y]
}

function assoc(x, y) {
    // Lookup atom in environment.
    const type = lisp_type(x)
    if(type == T_NUMBER) return x
    if(type == T_BOOL) return x
    if(type == T_SYMBOL) {
        if(eq(caar(y), x) === true) return cadar(y)
        else return assoc(x, cdr(y))
    }

    throw new Error(`unbound symbol: ${x}`)
}


/** Returns the second element of x. */
const cadr = (x) => car(cdr(x))
/** Returns the third element of x. */
const caddr = (x) => car(cdr(cdr(x)))
/** Returns the first element of the first element of x. */
const caar = (x) => car(car(x))
/** Returns the second element of the first element of x. */
const cadar = (x) => car(cdr(car(x)))



function evaluate(expression, environment) {
    if(atom(expression) === true) {
        return assoc(expression, environment)
    }
 
    const operator = car(expression)
    if(atom(operator) === true) {
        if(operator == 'quote') {
            return cadr(expression)
        } 
        else if(operator == 'atom') {
            return atom(evaluate(cadr(expression), environment))
        }
        else if(operator == 'eq') {
            return eq(
                evaluate(cadr(expression), environment),
                evaluate(caddr(expression), environment)
            )
        }
        else if(operator == 'car') {
            return car(
                evaluate(cadr(expression), environment)
            )
        }
        else if(operator == 'cdr') {
            return cdr(
                evaluate(cadr(expression), environment)
            )
        }
        else if(operator == 'cons') {
            return cons(
                evaluate(cadr(expression), environment),
                evaluate(caddr(expression), environment)
            )
        }
        else if(operator == 'cond') {
            return evcon(
                evaluate(cdr(expression), environment), environment
            )
        }
        else {
            const operands = cdr(expression)
            return evaluate(
                cons(
                    assoc(operator, environment), 
                    operands
                ), 
                environment
            )
        }
        // else return expression
    }
}

// ((test-a value-a) ... (test-n value-n))
function evcon(expression, environment) {
    if(evaluate(caar(expression), environment)) {
        return cadar(expression)
    }
    return evcon(cdr(expression), environment)
}



// const cond = () => {}

// // Environment 2.

// function _null(x) {
//     return eq(x, [])
// }

// function and(x,y) {
//     return cond(
//         [ x, cond(
//             [y, true], 
//             [true, []
//         ])],
//         [ true, [] ]
//     )
// }

// function not(x) {
//     return cond(
//         [x, []],
//         [true, true]
//     )
// }

// function append(x, y) {
//     return cond(
//         [_null(x), y],
//         [ true, 
//             cons([ 
//                 car(x), 
//                 append(cdr(x), y)
//             ])
//         ]
//     )
// }

// function pair(x, y) {
//     return 
//     // FIXME
//     if(and(_null(x), _null(y))) {
//         return []
//     } else if(not(atom(x)) && not(atom(y))) {
//         return cons(
//             list(car(x), car(y)),
//             pair(cdr(x), cdr(y))
//         )
//     }
// }



// // (list e_1...e_n) = (cons e_1 ... (cons e_n '())
// function list() {
//     return Array.from(arguments)
// }


const builtins = {
    quote: null,
    atom,
    eq,
    car,
    cdr,
    cons,
    cond: null,
    // cond,

    // pair,
    assoc,
    // apply,
    eval: evaluate,
    evcon,


    // TODO
    read
}



const env = Object.entries(builtins).map(([k, v]) => {
    if(typeof v === 'function') return [k, k]
    else return [k, v]
})

function run(source) {
    // Reader algorithm.
    try {
        // Read.
        const expr = read(source)
        // Evaluate.
        const returnValue = evaluate(expr[0], env)
        // Print.
        const returnValueAsString = inspect(returnValue, {colors: false, depth: Infinity})
        return returnValueAsString
    } catch(ex) {
        console.error(ex)
    }
}

const dumblisp = {
    eval_: run,
    builtins,
    env
}


module.exports = dumblisp
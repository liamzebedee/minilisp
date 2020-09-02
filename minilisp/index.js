const {inspect} = require('util');



// 
// LISP.
// 
const NIL = Symbol.for('nil')

const T_NUMBER = Symbol.for('nil')
const T_SYMBOL = Symbol.for('nil')
const T_BOOL = Symbol.for('nil')

function lisp_type(expr) {
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
            const atom = match[0].toUpperCase()
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



function quote(x) {
    return x
}

function atom(x) {
    if(
        typeof x == 'number' || 
        typeof x == 'string' ||
        typeof x == 'boolean' || 
        x == NIL) 
    return true

    if(typeof x == 'object' && x.constructor.name == 'Array') {
        if(x.length === 0) return true
        else return []
    }
}

function eq(x,y) {
    // deep eq
    if(atom(x) == true && atom(y) == true) {
        if(x === y) return true
    }

    if(x === [] && y === []) return true

    return []
}

function car(x) {
    if(x.length === 0) return NIL
    return x[0]
}

function cdr(x) {
    return x.slice(1)
}

function cons(x, y) {
    return [x, ...y]
}

function cond() {
    for(let arg of arguments) {
        if(evaluate(arg[0])) return arg[1]
    }
}




/** Returns the second element of x. */
const cadr = (x) => car(cdr(x))
/** Returns the third element of x. */
const caddr = (x) => car(cdr(cdr(x)))
/** Returns the first element of the first element of x. */
const caar = (x) => car(car(x))
/** Returns the second element of the first element of x. */
const cadar = (x) => car(cdr(car(x)))


/**
 * ((2 "Truthy") (t "Truthy")) ->
 *  ^----------- car(x)
 *     ^------- cdr(car(x))
 *      ------- car(cdr(car(x)))
 * @param {*} x 
 */
const cadar = (x) => car(cdr(car(x)))

function evaluate(expression, environment) {
    if(atom(expression) === true) {
        if(expression == NIL) return NIL

        // Lookup atom in environment.
        let type = lisp_type(expression)
        if(type == T_NUMBER) return expression
        if(type == T_BOOL) return expression

        if(type == T_SYMBOL) {

            // return expression
        }
        // TODO assoc(e, a)
    }
 
    const head = car(expression)
    if(atom(head) === true) {
        if(head == 'QUOTE') {
            return cadr(expression)
        } 
        else if(head == 'ATOM') {
            return atom(evaluate(cadr(expression), environment))
        }
        else if(head == 'EQ') {
            return eq(
                evaluate(cadr(expression), environment),
                evaluate(caddr(expression), environment)
            )
        }
        else if(head == 'CAR') {
            return car(
                evaluate(cadr(expression), environment)
            )
        }
        else if(head == 'CDR') {
            return cdr(
                evaluate(cadr(expression), environment)
            )
        }
        else if(head == 'CONS') {
            return cons(
                evaluate(cadr(expression), environment),
                evaluate(caddr(expression), environment)
            )
        }
        else if(head == 'COND') {
            return evcon(
                evaluate(cdr(expression), environment), environment
            )
        }
        
    }

    return expression
}

// ((test-a value-a) ... (test-n value-n))
function evcon(expression, environment) {
    if(evaluate(caar(expression), environment)) {
        return cadar(expression)
    }
    return evcon(cdr(expression), environment)
}





// Environment 2.

function _null(x) {
    return eq(x, [])
}

function and(x,y) {
    return cond(
        [ x, cond(
            [y, true], 
            [true, []
        ])],
        [ true, [] ]
    )
}

function not(x) {
    return cond(
        [x, []],
        [true, true]
    )
}

function append(x, y) {
    return cond(
        [_null(x), y],
        [ true, 
            cons([ 
                car(x), 
                append(cdr(x), y)
            ])
        ]
    )
}

function pair(x, y) {
    if(and(_null(x), _null(y))) {
        return []
    } else if(not(atom(x)) && not(atom(y))) {
        return cons(
            list(car(x), car(y)),
            pair(cdr(x), cdr(y))
        )
    }
}

function assoc() {

}

// (list e_1...e_n) = (cons e_1 ... (cons e_n '())
function list() {
    return Array.from(arguments)
}

const builtins = {
    quote,
    atom,
    eq,
    car,
    cdr,
    cons,
    cond,

    pair,
    assoc,
    // apply,
    eval: evaluate
}



const dumblisp = {
    eval_: function(source) {
        // Reader algorithm.
        try {
            // Parse expressions.
            const expr = read(source)
            const returnValue = evaluate(expr[0], builtins)
            const returnValueAsString = inspect(returnValue, {colors: false, depth: Infinity})
            return returnValueAsString
        } catch(ex) {
            console.error(ex)
        }
    },
    builtins
}


module.exports = dumblisp
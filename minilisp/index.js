const {inspect} = require('util');
function write(x) {
    console.log(inspect(x, {colors: false, depth: Infinity}))
}


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
            // TODO: refactor
            if(atom === 't') items.push(true)
            else if(atom === 'nil') items.push(NIL)
            else items.push(atom)

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

    return []
}

function car(x) {
    if(x.constructor.name !== 'Array') throw new Error("car expects a list")
    if(x.length === 0) return NIL
    return x[0]
}

/** Pops the head and returns the rest of x. */
function cdr(x) {
    if(x.constructor.name !== 'Array') throw new Error("cdr expects a list")
    if(x.length === 0) return NIL
    return x.slice(1)
}

function cons(x, y) {
    return [x, ...y]
}

function assoc(x, y) {
    // Numbers, booleans and nil are mapped to themselves.
    const type = lisp_type(x)
    if(type == T_NUMBER) return x
    if(type == T_BOOL) return x
    if(eq(x, NIL) === true) {
        return x
    }

    // Otherwise, lookup symbol in environment.
    let match = false
    for(let i = 0; i < y.length; i++) {
        if(eq(x, y[i][0]) === true) return y[i][1]
    }

    return NIL
}

/** Returns the second element of x. */
const cadr = (x) => car(cdr(x))
/** Returns the third element of x. */
const caddr = (x) => car(cdr(cdr(x)))
/** Returns the first element of the first element of x. */
const caar = (x) => car(car(x))
/** Returns the second element of the first element of x. */
const cadar = (x) => car(cdr(car(x)))


function extendEnvironment(environment, ext) {
    return [
        [key, value],
        environment
    ]
}
function evaluate(expression, environment = env) {
    if(!environment) throw new Error("environment undefined")

    if(atom(expression) === true) {
        return assoc(expression, environment)
    }

    const operator = car(expression)
    if(atom(operator) === true) {
        if(operator == 'quote') {
            // Special evaluation.
            if(expression.length !== 2) {
                throw new Error(`quote expects 1 arguments, ${expression.length - 1} given`)
            }
            return cadr(expression)
        } 
        else if(operator == 'atom') {
            if(expression.length !== 2) {
                throw new Error(`atom expects 1 argument, ${expression.length - 1} given`)
            }
            return atom(evaluate(cadr(expression), environment))
        }
        else if(operator == 'eq') {
            if(expression.length !== 3) {
                throw new Error(`eq expects 2 arguments, ${expression.length - 1} given`)
            }
            return eq(
                evaluate(cadr(expression), environment),
                evaluate(caddr(expression), environment)
            )
        }
        else if(operator == 'car') {
            if(expression.length !== 2) {
                throw new Error(`car expects 1 argument, ${expression.length - 1} given`)
            }
            return car(
                evaluate(cadr(expression), environment)
            )
        }
        else if(operator == 'cdr') {
            if(expression.length !== 2) {
                throw new Error(`cdr expects 1 argument, ${expression.length - 1} given`)
            }
            return cdr(
                evaluate(cadr(expression), environment)
            )
        }
        else if(operator == 'cons') {
            if(expression.length !== 3) {
                throw new Error(`cons expects 2 arguments, ${expression.length - 1} given`)
            }
            return cons(
                evaluate(cadr(expression), environment),
                evaluate(caddr(expression), environment)
            )
        }
        else if(operator == 'cond') {
            // Special evaluation.
            return evcon(
                evaluate(cdr(expression), environment), 
                environment
            )
        }
        // Arithmetic built-ins.
        else if(operator == '+') {
            const operands = evlist(cdr(expression), environment)
            return add(operands)
        }
        else if(operator == '-') {
            const operands = evlist(cdr(expression), environment)
            return subtract(operands)
        }
        else if(operator == 'write') {
            const operands = cdr(expression)
            const v = evaluate(operands, environment)
            return v
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
    else if(caar(expression) === 'lambda') {
        // ((lambda (p0 ... p_i) (body)) (a_0 ... a_i))
        // input: ((lambda (x) (cons x '(b))) 'a)
        // output: "(a b)"

        const lambdaExpr = car(expression)
        const parameters = cadr(lambdaExpr)
        const body = caddr(lambdaExpr)
        const callParameters = evlist(cdr(expression), environment)
        
        // To perform the call, the environment is extended with the call
        // data. (p_i -> a_i).
        if(parameters.length !== callParameters.length) {
            throw new Error(`lambda expects ${parameters.length} parameters, but ${callParameters.length} given`)
        }
        const callEnvironment = parameters.map((_, i) => {
            return [ parameters[i], callParameters[i] ]
        })

        const environmentExtended = [
            ...callEnvironment,
            ...environment
        ]
        
        return evaluate(body, environmentExtended)
    }
}

// ((test-a value-a) ... (test-n value-n))
function evcon(expression, environment) {
    if(!environment) throw new Error("environment undefined")
    if(evaluate(caar(expression), environment) === true) {
        return evaluate(cadar(expression), environment)
    }
    return evcon(cdr(expression), environment)
}

function evlist(expression, environment) {
    if(!environment) throw new Error("environment undefined")
    return cons(
        evaluate(car(expression), environment),
        evlist(cdr(expression), environment)
    )
}


// Environment 2.
function null_(x) {
    return eq(x, []) === true
}
function add(args) {
    return args.reduce((acc, x) => acc + x, 0)
}
function subtract(args) {
    return cdr(args).reduce((acc, x) => acc - x, car(args))
}


const builtins = {
    quote: 'quote',
    atom,
    eq,
    car,
    cdr,
    cons,
    cond: 'cond',

    assoc,
    '+': add,
    '-': subtract,
    '=': eq,


    // TODO
    read,
    evlist,
    evcon
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
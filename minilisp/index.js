


// 
// LISP.
// 
const NIL = Symbol.for('nil')

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


function parseSexpr(source, from = 0, d = 0) {
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
            const res = parseSexpr(source, i + 1, d + 1)
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
            items.push(atom)
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

const lisp = {
    eval: function(source) {
        // Reader algorithm.
        try {
            // Parse expressions.
            const res = parseSexpr(source)
            console.log(res)
        } catch(ex) {
            console.error(ex)
        }
    }
}

// 
// REPL.
// 

const readline = require('readline')

const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
})

async function run() {
    while(1) {
        // READ.
        const line = await new Promise((res,rej) => {
            rl.question('dumblisp> ', res)
        })
        // EVAL.
        with(lisp) {
            const res = eval(line)
            // PRINT
            console.log(res)
        }
    } // LOOP
}

run()

process.on('SIGINT', function() {
    rl.close()
    process.exit(0)
})


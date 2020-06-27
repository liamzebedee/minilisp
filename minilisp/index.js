// 
// LISP.
// 


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
function parseSexpr(source, from = 0) {
    console.log(source)

    // An S-Expression is a very simple form.
    // SEXPR = "(" SEXPR | ATOM ")"
    // ATOM = STRING | INTEGER | FLOAT | SYMBOL
    // STRING = '"' * '"'
    // INTEGER = [-][\d]
    // FLOAT = [-][\d+][.][\d+]
    // SYMBOL = [a-zA-Z\-0-9]
    let expr
    let atoms = []
    let acc = ""
    
    for(let i = 0 + from; i < source.length; i++) {
        let char = source[i]

        // S-Expressions.
        if(char == SEXPR_START) {
            // Find the end of this expression.
            const endIdx = source.slice(i).lastIndexOf(SEXPR_END)
            if(endIdx == -1) {
                throw new Error(`error parsing s-expr "${source}", expected terminating paren )`)
            }

            const str = source.substring(i + 1, endIdx)
            const atom = parseSexpr(str)
            atoms.push(atom)
            i = endIdx
            continue
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
            const atom = source.substring(i + 1, endIdx)
            atoms.push(atom)
            i = endIdx
            continue
        }

        // INTEGER/FLOAT
        const INTEGER_FLOAT_REGEX = /^[\+\-]?\d*\.?\d+(?:[Ee][\+\-]?\d+)?$/
        const integerFloatMatches = char.match(INTEGER_FLOAT_REGEX)
        if(integerFloatMatches) {
            // Now we seek to match the whole number pattern.
            const match = source.substring(i).match(INTEGER_FLOAT_REGEX)
            const atom = match[0]
            atoms.push(atom)
            i = i + atom.length
            continue
        }

        // SYMBOL

        if(char.match(/\s\r/)) continue
        if(char == "") return []
    }

    return atoms
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


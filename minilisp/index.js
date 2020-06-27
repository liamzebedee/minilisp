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
        const res = eval(line)
        // PRINT
        console.log(res)
    } // LOOP
}

run()

process.on('SIGINT', function() {
    rl.close()
    process.exit(0)
})


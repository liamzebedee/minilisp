const readline = require('readline')

const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
})

async function run() {
    while(1) {
        const line = await new Promise((res,rej) => {
            rl.question('dumblisp> ', res)
        })
        console.log(`echo ${line}`)
    }
}

run()

process.on('SIGINT', function() {
    rl.close()
    process.exit(0)
})


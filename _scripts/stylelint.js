const fs = require('fs')
const path = require('path')

const glob = require('glob')
const pangu = require('pangu')

const entry = path.join(__dirname, '../')

const main = {
  init(dir) {
    glob(`${dir}/**/*.md`, { nodir: true, ignore: '**/node_modules/**' }, (err, files) => {
      if (err) {
        throw err
      }

      for (const file of files) {
        let content = fs.readFileSync(file).toString()
        content = pangu.spacing(content)

        // other lint

        fs.writeFileSync(file, content)
        const fileName = file.split('/').pop()
        console.log('lint', fileName)
      }
    })
  },
}

main.init(entry)

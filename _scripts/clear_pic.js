const glob = require('glob')
const fs = require('fs')
const path = require('path')

const img = path.join(__dirname, '../**/**.{png,jpg,gif,jpeg}')
const md = path.join(__dirname, '../**/**.md')

const imgs = glob.sync(img, { ignore: '_book' })
console.log(imgs)
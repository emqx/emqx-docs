const fs = require('fs')
const path = require('path')
const argv = require('yargs').argv
const e = ['emqxee', 'emqxce'].includes(argv.e) ? argv.e : 'emqxce'

// Copy SUMMARY
let summary = 'SUMMARY_CE.md'
let cover = 'cover-emqx-ce.jpg'
if (e === 'emqxee') {
  summary = 'SUMMARY_EE.md'
  cover = 'cover-emqx-ee.jpg'
}
const targetSummary = path.join(__dirname, '../SUMMARY.md')
const targetCover = path.join(__dirname, '../cover.jpg')
fs.copyFileSync(
  path.join(__dirname, '../', summary),
  targetSummary,
)
fs.copyFileSync(
  path.join(__dirname, '../', cover),
  targetCover,
)
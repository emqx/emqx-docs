const fs = require('fs')
const path = require('path')
const argv = require('yargs').argv
const e = ['emqxee', 'emqxce'].includes(argv.e) ? argv.e : 'emqxce'

// Copy SUMMARY
let summary = 'SUMMARY_CE.md'
if (e === 'emqxee') {
  summary = 'SUMMARY_EE.md'
}
const targetSummary = path.join(__dirname, '../SUMMARY.md')
fs.copyFileSync(
  path.join(__dirname, '../', summary),
  targetSummary,
)
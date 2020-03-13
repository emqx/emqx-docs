const fs = require('fs')
const path = require('path')
const dateFormat = require('dateformat')

const summary = require('./summary.json')
const DOC_DIR = path.join(__dirname, '../')

function getRealFile(file = '') {
  return file.replace(/#$/, '')
}

function getTpl() {
  const tplPath = path.join(DOC_DIR, '_scaffolds/content.md')
  return fs.readFileSync(tplPath).toString()
}

function now() {
  return dateFormat('yyyy-mm-dd HH:MM:ss')
}

function writeOrCreateAndWrite(filepath = '', content = '') {
  const pathArr = filepath.split('/')
  pathArr.pop()
  const dirname = pathArr.join('/')
  if (!fs.existsSync(dirname)) {
    require('child_process').execSync(`mkdir -p ${dirname}`)
  }
  fs.writeFileSync(filepath, content)
}

function replaceTpl(config = {}, tpl = '') {
  let tplTemp = tpl
  Object.entries(config).forEach(([key, value]) => {
    tplTemp = tplTemp.replace(new RegExp(`{{ ${key} }}`, 'gim'), value)
  })
  return tplTemp
}

const tpl = getTpl()
const summaryArr = ['# SUMMARY']
function initPost(item = {}, config = {}, deep = 0) {
  const {  file, title } = item
  // Join path and remove hash tag
  const filepath = path.join(DOC_DIR, getRealFile(file))
  // Generate file content
  const realConfig = {
    ...config,
    date: now(),
    title: title,
    ref: item.ref,
  }
  const content = replaceTpl(realConfig, tpl)
  writeOrCreateAndWrite(filepath, content)
  summaryArr.push(`${' '.repeat(deep * 2)}* [${item.title}](${item.file})`)
  if (item.child && item.child.length > 0) {
    loop(item.child, config, deep + 1)
  }
}

const config = {
  title: '',
  author: 'wivwiv',
  data: '',
  category: '',
  ref: '',
}

function loop(data = [], config, deep = 0) {
  data.forEach((item) => {
    initPost(item, config, deep)
  })
}

loop(summary, config)

const summaryFile = path.join(DOC_DIR, 'SUMMARY.md')
fs.writeFileSync(summaryFile, summaryArr.join('\n'))
console.log('Done')

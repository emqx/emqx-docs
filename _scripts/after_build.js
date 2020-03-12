const path = require('path');
const fs = require('fs');
const { execSync } = require('child_process');

const _build = path.join(__dirname, '../_book');
const rmList = ['_scaffolds', '_js', '_scripts', '_styles', 'package.json', 'README.md', 'yarn.lock', '.gitignore'];

rmList.forEach(name => {
  try {
    const file = path.join(_build, name)
    execSync(`rm -rf ${file}`);
  } catch (e) {
    console.log(e)
  }
})

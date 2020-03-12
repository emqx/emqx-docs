// copy to https://docs.emqx.io/cn/sdk_tools
var a = document.querySelectorAll('.sdktools .col-md-3')
var list = []
a.forEach(el => {
 var item = {
  img: el.querySelector('img').src,
  txt: el.querySelector('.sdktools-item__footer').innerText,
  href: el.querySelector('a').href
 }
list.push(item)
})

var dd = list.map(item => {
return `
  <a class="hardware-item" href="${item.href}" target="_blank">
    <img src="${item.img}" />
    <div class="hardware-name">${item.txt}</div>
  </a>
`
})
var content = `<div class="development-hardware-warp">
${dd.join('\n')}
</div>
`
console.log(content)
copy(content)
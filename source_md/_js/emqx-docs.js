function fixTabs() {
  var a = document.querySelectorAll('.nav-tabs li[role="presentation"] a')
  a.forEach(function(el, i) {
    var tag = el.getAttribute('href')
    if (!tag) {
      return
    }
    tag += '-' + i
    el.setAttribute('href', tag)
   })
}

gitbook.events.bind('page.change', function() {
  fixTabs()
})
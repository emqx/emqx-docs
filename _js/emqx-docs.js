function fixTabs() {
  var comment = $('#gitalk-container');
  var __i = 0
  var __timer = setInterval(() => {
    __i += 1
    if (__i >= 10000) {
      clearInterval(__timer)
    }
    var user = $('#gitalk-container .gt-user')
    if (comment && user && user[0]) {
      $('.gt-header').slideUp()
      $('.gt-comments').slideUp()
      clearInterval(__timer)
      var __el0 = document.createElement('div');
      __el0.setAttribute('class', 'gt-user-inner')
      __el0.setAttribute('id', 'wrap-btn')
      var __el = document.createElement('span');
      __el.setAttribute('class', 'gt-user-name')
      __el0.innerText = '展开'
      // var __el2 = document.createElement('span')
      // __el2.setAttribute('class', 'gt-ico gt-ico-arrdown')
      // __el0.appendChild(__el)
      // __el0.appendChild(__el2)
      user[0].after(__el0)
      setTimeout(function() {
        var btn = $('#wrap-btn')
        btn.click(function() {
          var a = $('.gt-header')
          var b = $('.gt-comments')
          a.slideToggle()
          b.slideToggle(function() {
            if ($('.gt-comments').css('display') === 'none') {
              btn.text('展开')
            } else {
              btn.text('折叠')
            }
          })
        })
      }, 10)
    }
  }, 100)

  var a = document.querySelectorAll('.nav-tabs li[role="presentation"] a')
  a.forEach(function(el, i) {
    var tag = el.getAttribute('href')
    if (!tag) {
      return
    }
    tag = '#' + tag
    el.setAttribute('href', tag)
   })
}

gitbook.events.bind('page.change', function() {
  fixTabs()
})
function fixTabs() {
  var comment = $('#gitalk-container');
  if (comment && !$('#show-btn').text()) {
    comment.before('<div class="hide-wrap"><button id="show-btn">查看评论</button></div>')
    setTimeout(function() {
      var btn = $('#show-btn')
      btn.click(function() {
        comment.slideToggle(function() {
          if ($('#gitalk-container').css('display') === 'none') {
            btn.text('查看评论')
          } else {
            btn.text('收起评论')
          }
        })
      })
    }, 10)
  }
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
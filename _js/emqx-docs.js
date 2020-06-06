function fixTabs() {
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

if (location.host === 'docs.emqx.io') {
  var _hmt = _hmt || [];
  (function() {
    var hm = document.createElement("script");
    hm.src = "https://hm.baidu.com/hm.js?7654abcb7de209cbcee5f56f7874a00b";
    var s = document.getElementsByTagName("script")[0]; 
    s.parentNode.insertBefore(hm, s);
  })();
}

gitbook.events.bind('page.change', function() {
  fixTabs()
  setTimeout(function() {
    try {
      if (window._hmt) {
        var title = document.title.split(' · ')[0]
        var pathNameList = window.location.pathname.split('/')
        var currentDocType = pathNameList[1]
        var currentLanguage = pathNameList[3]
        var currentVersion = pathNameList[2]
        if (currentVersion === 'latest') {
          currentVersion = document.querySelector('#version-select option')
            .innerText.replace(' (latest)', '')
        }
        // every page pv
        window._hmt.push(['_trackEvent', currentDocType, currentVersion + '-' + currentLanguage, title])
      }
    } catch (e) {
      console.error('track error', e)
    }
  }, 4000)
})
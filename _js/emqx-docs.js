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

if (location.host === 'docs.emqx.net') {
  var _hmt = _hmt || [];
  (function() {
    var hm = document.createElement("script");
    hm.src = "https://hm.baidu.com/hm.js?0853f370054d89aeee03219c2043f3f2";
    var s = document.getElementsByTagName("script")[0]; 
    s.parentNode.insertBefore(hm, s);
  })();
} else if (location.host === 'docs.emqx.io') {
  var _hmt = _hmt || [];
  (function() {
    var hm = document.createElement("script");
    hm.src = "https://hm.baidu.com/hm.js?1e6bddec74891608f3ffcb2d43cf458d";
    var s = document.getElementsByTagName("script")[0]; 
    s.parentNode.insertBefore(hm, s);
  })();
}

function fixHash() {
  var li = document.querySelector('li .chapter.active')
  if (li.parentNode && li.parentNode.parentNode) {
    $(li.parentNode.parentNode).addClass('cuav-expanded')
  }
}

gitbook.events.bind('page.change', function() {
  fixTabs()
  setTimeout(() => {
    fixHash()
  }, 100)
  var times = localStorage.getItem('emqx_wenjuan')
  times = parseInt(times, 10) || 0
  if (times <= 3 && !document.querySelector('#emqx_wenjuan')) {
    var html = '<a target="_blank" id="emqx_wenjuan" style="border: 1px solid white;box-shadow: 0 1px 3px 0 rgba(0, 0, 0, 0.3);border-radius: 2px 2px 2px 2px;font-size: 12px;line-height: 14px;position:fixed;z-index:999;display: inline-block;width: 25px;word-wrap: break-word;padding: 10px 6px;color: #FFFFFF; background: #34c388; right: 0; bottom: 20px;" href="https://jinshuju.net/f/uSbPs5">问卷调查</a>'
    var a = document.createElement('a')
    a.innerHTML = html
    a.onclick = function() {
      times += 1
      localStorage.setItem('emqx_wenjuan', times)
    }
    document.querySelector('body').appendChild(
      a    
    )
  }
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

// init
function __init() {
  var hash = location.hash
  setTimeout(() => {
    location.hash = hash
    console.log('rest href')
  }, 100)
}
__init()
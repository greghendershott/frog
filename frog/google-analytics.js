var _gaq = _gaq || [];
_gaq.push(['_setAccount', '~a']);
_gaq.push(['_setDomainName', '~a']);
_gaq.push(['_trackPageview']);

// http://drawingablank.me/blog/fix-your-bounce-rate.html
setTimeout(function(){_gaq.push(['_trackEvent', '30_seconds', 'read'])},
           30000);

(function() {
    var ga = document.createElement('script');
    ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0];
    s.parentNode.insertBefore(ga, s);
})();

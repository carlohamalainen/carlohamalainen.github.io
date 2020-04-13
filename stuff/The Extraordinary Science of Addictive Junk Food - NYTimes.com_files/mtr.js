/*
$Id: mtr.js 123429 2013-03-27 17:30:29Z mmenezes $
(c)2011-12 The New York Times Company
*/

var NYTD = NYTD || {};

NYTD.Meter = {};

/**
 * 
 * mtr.js
 * 
 * Exposes meter functionality
 * Calls gateway and Growl
 * 
 * External calls
 *      Global NYTD method:
 *      NYTD.Meter.check({
 *          url: {String}, // Optional, the canonical url of the page; defaults to location.href 
 *          callback: {Function}, // Optional, notifies when meter responds, recommended for ajax pages
 *      })
 */
(function(){
    
    'use strict'
    /*jslint vars: true*/
    
    // consumers can pass a callback for when the meter service is loaded
    var externalCheckMeterCallback = null;

    var callbackName = String(String.fromCharCode(97 + Math.round(Math.random() * 25))+(new Date()).getTime());
    var head = document.getElementsByTagName('head')[0];
    var hash, cookieHash;
    
    function killEsc(e) {
        if (!e) {
            // for IE
            e = window.event;
        }

        var code;
        if (e.keyCode) {
            // for IE
            code = e.keyCode;
        }
        else if (e.which) {
            // for other browsers
            code = e.which;
        }

        if (code === 27) {
            e.preventDefault();
            return false;
        }

        return true;
    }
    var listenForKeyDown = {
        add: function(callback) {
            if (typeof window.addEventListener !== 'undefined') {
                window.addEventListener('keydown', callback, false);
            }
            else if (typeof window.attachEvent !== 'undefined') {
                window.attachEvent('onkeydown', callback);
            }
        },
        remove: function(func) {
            if (typeof window.addEventListener !== 'undefined') {
                window.removeEventListener('keydown', func, false);
            }
            else if (typeof window.attachEvent !== 'undefined') {
                window.detachEvent('onkeydown', func);
            }
        }
    };

    listenForKeyDown.add(killEsc);

    function hasLocalStorage() {
        // excludes IE8 users due to bug 78654
        if (/MSIE 8.0/.test(navigator.userAgent)) {
            return false;
        }
        try {
            return 'localStorage' in window && window['localStorage'] !== null;
        } catch(e){
            return false;
        }
    }

    function getCookie() {
        var ret = /NYT-M=([^;]+)/i.test(unescape(document.cookie)) ? RegExp.$1 : '';
        if (!ret) {
            try {
                ret = hasLocalStorage() && window.localStorage.getItem("nyt-m");
            }
            catch(e) {
            // fails silently
            }
            if (!ret || !ret.match(/e=i./)) {
                ret = window.name;
            }
            document.cookie="nyt-m=" + ret + ";path=/; domain=.nytimes.com";
        }
        backupCookie(ret);
        return ret;
    }

    function backupCookie(cookie) {
        window.name = cookie;
        if(hasLocalStorage()){
            try {
                window.localStorage.setItem("nyt-m", cookie);
            }
            catch(e) {
            // fails silently
            }
        }
    }
    
    function addMeta(name, value) {
        var meta = document.createElement('meta');
        meta.name = name;
        meta.content = value;
        head.appendChild(meta);
    }
    
    function addScript(src) {
        var script = document.createElement('script');
        script.src = src;
        script.async = true;
        head.appendChild(script);
    }

    function getHash() {
        return (/gwh=([^&]+)/.test(unescape(window.location.search.substring(1))) ? RegExp.$1 : '');
    }

    function removeHash() {
        window.location.replace(window.location.href.replace(/(\?|&)gwh=([^&]+)/, ''));
    }
    
    function isErrorPage() {
        var pst = document.getElementsByName("PST")[0];
        var title = document.getElementsByTagName('title')[0];
        return (pst && pst.content && pst.content.match(/error/i)) || (title && title.innerHTML.match(/Page Not Found/i));
    }
    
    function setMeterLoaded(val) {
        if (NYTD && NYTD.Meter) {
            NYTD.Meter.loaded = val;
        } 
    }
    
    function dispatchEvent(evtName) {
        var $ = (NYTD && NYTD.jQuery) ? NYTD.jQuery : jQuery;
        if ($) {
            $(document).trigger(evtName);
        }
    } 

    function checkMeter(options) {
        if (!options) {
            options = {};
        }
        // backwards compatibility for video
        else if (typeof options === 'string') {
            options = {
                url: options
            };
        }
        // end backwards compatibility for video
        else if (options.callback) externalCheckMeterCallback = options.callback;

        if (isErrorPage()) {
            listenForKeyDown.remove(killEsc);
        }
        else {
            
            setMeterLoaded(false);
            
            var referrer = options.url ? (document.referrer || window.location.href) : document.referrer;
            var serviceUrl = '//meter-svc.nytimes.com/meter.js?url=' + encodeURIComponent(options.url || location.href) + '&referer=' + encodeURIComponent(referrer) + '&callback=' + callbackName;
            window[callbackName] = processMeterResponse;
            addScript(serviceUrl);
        }
    }

    function processMeterResponse(response) {
        
        setMeterLoaded(true);
        
        if (response.counted) {
            addMeta('WT.z_cad', '1');
        }
        if (response.hitPaywall) {
            var anchor = unescape(document.location.href).split('#');
            anchor = anchor.length > 1 ? '#' + anchor[1] : null;
            var hash = 'gwh=' + response.hash;
            var url = window.location.search ? window.location.href + '&' + hash :  window.location.href + '?' + hash;
            url = anchor ? url.replace(anchor, '') + anchor : url;
            window.location.replace(url);
        }
        else {
            var cookie = getCookie();
            var match = cookie.match(/v=i.([0-9])/);
            var meterPageCount = (match && match[1]) ? parseInt(match[1], 10) : null;
            listenForKeyDown.remove(killEsc);
            if (typeof externalCheckMeterCallback === 'function') {
                externalCheckMeterCallback({
                    hitPaywall: response.hitPaywall,
                    count: meterPageCount
                });
            }
            NYTD.Meter.pageCount = meterPageCount;
            externalCheckMeterCallback = null;
            dispatchEvent('NYTD:MeterLoaded');
        }
    }
    
    function addGatewayScript() {
        if (location.hostname in {
            "myaccount.nytimes.com":1
        }) {
            return;
        }

        track();
        NYTD.Meter.gwy = true;
        var script = document.createElement('script');
        script.src = NYTD.Hosts.jsHost + '/js/gwy.js';
        head.appendChild(script);
    }

    function track() {
        mtr_track(
            "WT.cg_n", "Digital Subscription",
            "WT.cg_s", "",
            "WT.z_gpt", "E-Commerce",
            "WT.si_n", "Digital Subscription",
            "WT.si_x", "1",
            "WT.z_gpst", "Purchase",
            'WT.ti', 'Gateway',
            'DCS.dcssip', 'myaccount.nytimes.com',
            'DCS.dcsuri', '/mem/purchase/gateway'
            );
    }

    function mtr_track() {
        if ('dcsMultiTrack' in window) {
            var old_dcsid = dcsInit.dcsid;
            dcsInit.dcsid = wt_dcsidArray["Digital Subscription"];
            dcsMultiTrack.apply(this, arguments);
            dcsInit.dcsid = old_dcsid;
        } else {
            setTimeout(function() {
                mtr_track.apply(this, arguments);
            }, 1000);
        }
    }
    
    function loadGateway() {
        if (document.getElementsByTagName('body').length > 0) {
            addGatewayScript();
        }
        else {
            setTimeout(function() {
                loadGateway();
            }, 100);
        }
    }

    hash = getHash();
    cookieHash = getCookie().substr(0,32);

    if (!hash) {
        checkMeter();
    } else if (hash && !cookieHash || hash !== cookieHash ) {
        removeHash();
    } else if (hash && cookieHash && hash === cookieHash) {
        loadGateway();
    }

    NYTD.Meter.check = checkMeter;  

})();


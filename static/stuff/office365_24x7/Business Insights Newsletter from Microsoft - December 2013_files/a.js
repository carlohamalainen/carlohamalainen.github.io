DynamicsMarketing = {
    referrer: document.referrer,
    location: document.location,
    protocol: location.protocol,
    host: location.host,
    pathname: location.pathname,
    search: location.search,
    key: '',
    sid: '',
    QueryString: function () {
        var q = "rid=" + this.E(Math.floor(Math.random() * 9999999999));
        q += "&key=" + this.E(this.key);
        q += "&referrer=" + this.E(this.referrer);
        q += "&location=" + this.E(this.location);
        q += "&protocol=" + this.E(this.protocol);
        q += "&host=" + this.E(this.host);
        q += "&pathname=" + this.E(this.pathname);
        q += "&search=" + this.E(this.search);
        q += "&sid=" + this.E(this.sid);
        return q;
    },
    A: function (btid, btserver, bsid, s) {
        this.key = btid;
        this.sid = bsid;
        if (s) {
            this.search = s;
        }

        var i = new Image();
        i.id = "i" + btid;
        i.width = "0px";
        i.height = "0px";
        i.onload = function () {
            try {
                var target = document.getElementById('d' + btid);
                if (target != null) {
                    target.width = "0px";
                    target.height = "0px";
                    target.appendChild(i);
                }
            } catch (ex) {
            }
        };
        i.src = this.protocol + "//" + btserver + "cs.do?" + this.QueryString();
        
    },
    E: function (s) {
        return encodeURIComponent(s);
    }
};

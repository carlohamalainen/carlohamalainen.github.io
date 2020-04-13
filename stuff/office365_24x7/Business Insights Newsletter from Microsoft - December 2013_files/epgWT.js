/**** START - FOR WEBTRENDS TAGGING SCRIPT ****/
function epgWTLocaleFunction() {
    url = window.location.pathname.toLowerCase();
    if (url.charAt(0) == "/") url = url.slice(1, url.length);

    if (url.replace('enterprise/', '').split("/", 2)[0] != undefined) {
        if (url.replace('enterprise/', '').split("/", 2)[0].toLowerCase() == "romania") {
            return "Romania";
        }
        if (url.replace('enterprise/', '').split("/", 2)[0].indexOf("-") > -1 && url.replace('enterprise/', '').split("/", 2)[0].length < 7)
            return url.replace('enterprise/', '').split("/", 2)[0];
        return "EN-US";
    }
    return "None";
}
function epgWTChannelFunction() {
    url = window.location.pathname.toLowerCase();
    if (url.charAt(0) == "/") url = url.slice(1, url.length);

    if (epgWTLocaleFunction() == "EN-US") {
        if (url.replace('enterprise/', '').split("/", 2)[0].indexOf(".") > -1 || url == undefined)
            return "None";
        return url.replace('enterprise/', '').split("/", 2)[0];
    } else if (epgWTLocaleFunction() == "Romania") {
        return "None";
    } else {
        if (url.replace('enterprise/', '').replace(epgWTLocaleFunction() + '/', '').split("/", 2)[0].indexOf(".") > -1 || url == undefined)
            return "None";
        return url.replace('enterprise/', '').replace(epgWTLocaleFunction() + '/', '').split("/", 2)[0];
    }
}

function epgWTSubChannelFunction() {
    url = window.location.pathname.toLowerCase();
    if (url.charAt(0) == "/") url = url.slice(1, url.length);

    if (epgWTLocaleFunction() == "EN-US") {
        var channel = url.replace('enterprise/', '').split("/", 2)[0];
        var subchannel = url.replace('enterprise/', '').split("/", 2)[1];
        if (channel == "event" || channel == "microsoft-it" || channel == "partners" || channel == undefined || subchannel == undefined || subchannel.indexOf(".") > -1)
            return "None";
        else if (channel == "industry")
            return url.replace('enterprise/', '').replace('industry/').split("/", 2)[1].toLowerCase();
        return subchannel;
    } else {
        var channel = url.replace('enterprise/', '').replace(epgWTLocaleFunction() + '/', '').split("/", 2)[0];
        var subchannel = url.replace('enterprise/', '').replace(epgWTLocaleFunction() + '/', '').split("/", 2)[1];
        if (channel == "event" || channel == "microsoft-it" || channel == "partners" || channel == undefined || subchannel == undefined || subchannel.indexOf(".") > -1)
            return "None";
        else if (channel == "industry")
            return url.replace('enterprise/', '').replace(epgWTLocaleFunction() + '/', '').replace('industry/').split("/", 2)[1].toLowerCase();
        return subchannel;
    }
}

function scanMetaTags() {
    var metas = document.getElementsByTagName('meta');
    for (var ctr = 0; ctr < metas.length; ctr++) {
        if (metas[ctr].getAttribute("name") == "WT.z_enterprise") {
            z_enterprise_Exists = true;
            metas[ctr].setAttribute("content", "1");
        } else if (metas[ctr].getAttribute("name") == "WT.cg_n") {
            cg_n_Exists = true;
            metas[ctr].setAttribute("content", epgWTLocaleFunction());
        } else if (metas[ctr].getAttribute("name") == "WT.cg_s") {
            cg_s_Exists = true;
            metas[ctr].setAttribute("content", epgWTChannelFunction());
        } else if (metas[ctr].getAttribute("name") == "WT.cg_ss") {
            cg_ss_Exists = true;
            metas[ctr].setAttribute("content", epgWTSubChannelFunction());
        } else if (metas[ctr].getAttribute("name") == "WT.pi") {
            pi_Exists = true;
            metas[ctr].setAttribute("content", epgWTLocaleFunction() + "|" + epgWTChannelFunction() + "|" + epgWTSubChannelFunction());
        } else if (metas[ctr].getAttribute("name") == "DCSdir.ClearVars") {
            DCSdir_Exists = true;
            metas[ctr].setAttribute("content", "WT.z_content, WT.z_evt, WT.clip_n, WT.clip_ev, WT.clip_t, WT.clip_tv, WT.clip_q");
        } else if (metas[ctr].getAttribute("name") == "DCS.dirClearVars") {
            metas[ctr].setAttribute("content", "");
        } else if (metas[ctr].getAttribute("name") == "WT.clip_n") {
            metas[ctr].setAttribute("content", metas[ctr].getAttribute("content").trim());
        }
    }
}

function z_ContentFunction() {
    url = window.location.pathname.toLowerCase();
    if (url.charAt(0) == "/") url = url.slice(1, url.length);

    if (url.indexOf("/articles/") > -1 || url.indexOf("/artikels/") > -1) {
        var articleMeta = document.createElement('meta');
        articleMeta.name = "WT.z_content";
        articleMeta.content = "Article";
        document.getElementsByTagName('head')[0].appendChild(articleMeta);
    } else if (url.indexOf("/newsletters/") > -1) {
        var articleMeta = document.createElement('meta');
        articleMeta.name = "WT.z_content";
        articleMeta.content = "Newsletter";
        document.getElementsByTagName('head')[0].appendChild(articleMeta);
    } else if (url.indexOf("/event/") > -1) {
        var articleMeta = document.createElement('meta');
        articleMeta.name = "WT.z_content";
        articleMeta.content = "Event";
        document.getElementsByTagName('head')[0].appendChild(articleMeta);
    }
}

function z_CustomerRefFunction() {
    $.get("http://www.microsoft.com/global/enterprise/renderingassets/js/CustomerReference.txt", function (data) {
        var customerRefList = data.split('\n');
        for (var itemCtr = 0 ; itemCtr < customerRefList.length; itemCtr++) {
            if (customerRefList[itemCtr] != undefined || customerRefList[itemCtr] != "") {
                if (customerRefList[itemCtr].toLowerCase().indexOf(url) > -1) {
                    var articleMeta = document.createElement('meta');
                    articleMeta.name = "WT.z_customerref";
                    articleMeta.content = "1";
                    document.getElementsByTagName('head')[0].appendChild(articleMeta);
                    break;
                }
            }
        }
    });
}

var url = window.location.pathname.toLowerCase();
if (url.charAt(0) == "/") url = url.slice(1, url.length);

var z_enterprise_Exists = false;
var cg_n_Exists = false;
var cg_s_Exists = false;
var cg_ss_Exists = false;
var pi_Exists = false;
var DCSdir_Exists = false;
var clip_n_Exists = false;

scanMetaTags();
z_ContentFunction();

if (!z_enterprise_Exists) {
    var enterpriseMeta = document.createElement('meta');
    enterpriseMeta.name = "WT.z_enterprise";
    enterpriseMeta.content = "1";
    document.getElementsByTagName('head')[0].appendChild(enterpriseMeta);
}

if (!cg_n_Exists) {
    var localeMeta = document.createElement('meta');
    localeMeta.name = "WT.cg_n";
    localeMeta.content = epgWTLocaleFunction();
    document.getElementsByTagName('head')[0].appendChild(localeMeta);
}

if (!cg_s_Exists) {
    var channelMeta = document.createElement('meta');
    channelMeta.name = "WT.cg_s";
    channelMeta.content = epgWTChannelFunction();
    document.getElementsByTagName('head')[0].appendChild(channelMeta);
}

if (!cg_ss_Exists) {
    var subchannelMeta = document.createElement('meta');
    subchannelMeta.name = "WT.cg_ss";
    subchannelMeta.content = epgWTSubChannelFunction();
    document.getElementsByTagName('head')[0].appendChild(subchannelMeta);
}

if (!pi_Exists) {
    var lcsMeta = document.createElement('meta');
    lcsMeta.name = "WT.pi";
    lcsMeta.content = epgWTLocaleFunction() + "|" + epgWTChannelFunction() + "|" + epgWTSubChannelFunction();
    document.getElementsByTagName('head')[0].appendChild(lcsMeta);
}

if (!DCSdir_Exists) {
    var clearVarsMeta = document.createElement('meta');
    clearVarsMeta.name = "DCSdir.ClearVars";
    clearVarsMeta.content = "WT.z_content,WT.z_evt,WT.clip_n,WT.clip_ev,WT.clip_t,WT.clip_tv,WT.clip_q";
    document.getElementsByTagName('head')[0].appendChild(clearVarsMeta);
}

/**** END - FOR WEBTRENDS TAGGING SCRIPT ****/

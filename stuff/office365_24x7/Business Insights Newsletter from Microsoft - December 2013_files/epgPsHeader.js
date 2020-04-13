(function ($) {
    $.fn.replaceHeader = function (options) {
        var container = $(this);
        var defaults = {
            options: [{ href: "/enterprise/default.aspx", text: "Some Sector" }]
        };

        var semaphor = true;

        $.extend(defaults, options);

        var getReplacementHtml = function () {
            if (!defaults.options.length) { // If there's no options to choose from, don't replace the html.
                return null;
            }

            var html = "<div class='epgPSHeader'>";
            html += "<div class='currentSite'>";
            html += "<a href='" + defaults.options[0].href + "' title='" + defaults.options[0].text + "'>" + defaults.options[0].text + "</a>";
            html += "</div>";

            if (defaults.options.length === 1) { // If there's only one option, only generate the header, not the dropdown options.
                return html + "</div>";
            }
            html += "<div class='dropdownCTA icon-downarr'></div>";
            html += "<div class='headerFlyout'>";
            html += "<ul>";
            for (var i = 1; i < defaults.options.length; i++) {
                html += "<li class='headerOption'>";
                html += "<span class='" + defaults.options[i].clasa + "'></span><a href='" + defaults.options[i].href + "' title='" + defaults.options[i].text + "'>" + defaults.options[i].text + "</a>";
                html += "</li>";
            }
            html += "</ul>";
            html += "</div>";
            html += "</div>";

            return html; // Generate header + dropdown options.
        };

        var bindEvents = function () {

            $("body").on("click", function () { closeFlyout(); })
                .on("click", ".dropdownCTA", function (event) {

                    if (container.hasClass("selected")) {
                        closeFlyout()
                    } else {
                        openFlyout();
                    }

                    //return false;
                }).on("click", ".headerFlyout", function (event) { event.stopImmediatePropagation(); });
        }

        var init = function () {
            var html = getReplacementHtml();

            if (html) {// do this only if there were options to replace
                container.empty().html(html);
                initHeaderWidth();
                // Bind Events. Leave some timeout for the DOM to refresh. 
                window.setTimeout(function () {
                    bindEvents()
                }, 30);
            }
            //initHeaderWidth();
        }

        var closeFlyout = function () {
            if (!semaphor) {
                semaphor = false;
                window.setTimeout(function () {
                    $(container).find(".epgPSHeader").stop(true, true).animate({ backgroundColor: '#fff' }, 300, function () { semaphor = true; }).removeClass("selected");
                }, 400);
                container.removeClass("selected").find(".headerFlyout").slideUp("500", "linear");
            }
            
        }

        var openFlyout = function () {
            if (semaphor) {
                semaphor = true; // Leave this in. Prevents the nav from closing
                initHeaderWidth();
                //container.find(".headerFlyout").width($(container.find(".epgPSHeader")).width() - 1);
                container.find(".epgPSHeader").stop(true, true).addClass("selected");
                container.addClass("selected").find(".headerFlyout").slideDown("500");
            }
            window.setTimeout(function () { semaphor = false; }, 500);
        }
        var initHeaderWidth = function () {
            //can not use width(XX), because some site call jquery-1.8 ,there will be some differences between jquery-1.11 and jquery-1.8, when the element set box-sizing:border-box
            //var widthHeader = container.find(".epgPSHeader").width();
            var widthFlyout = container.find(".headerFlyout").width();
            var widthCurrent = $(".currentSite").width();
            var outerWidthCurrent = $(".currentSite").outerWidth();
            var outerWidthHeader = container.find(".epgPSHeader").outerWidth();
            var outerWidthFlyout = container.find(".headerFlyout").outerWidth();
            if (outerWidthHeader >= outerWidthFlyout) {
                $(".headerFlyout").css("width", outerWidthHeader);
            } else {
                if (widthFlyout < outerWidthCurrent + 24) {
                    outerWidthFlyout = outerWidthFlyout + outerWidthCurrent + 24 - widthFlyout;
                }

                var marginRight = widthFlyout - widthCurrent - 24;
                if ($("html").prop("dir").toUpperCase() === "RTL") {
                    $(".currentSite").css("padding-left", marginRight);
                } else {
                    $(".currentSite").css("padding-right", marginRight);
                }
                //some element's width contains decimal point, so plus 1.
                outerWidthFlyout = outerWidthFlyout + 1;
                $(".headerFlyout").css("width", outerWidthFlyout);
                $(".epgPSHeader").css("width", outerWidthFlyout);
                
            }

        }
        init();
    }

})(jQuery);
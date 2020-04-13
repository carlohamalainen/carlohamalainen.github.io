$(function () {
    var semaphor = true;

    var navElement = $(".mstHdr_SecRow .oneMscomNavV3");
    var allFlyouts = $(navElement).find(".mstNavNavItemFlyout");
    var menuTabs = $(navElement).find(".mstNavNavItem");
    $("body").on("click", function () {
        //if (!semaphor) {
        //    semaphor = false;
        $(".oneMscomNavV3 .mstNavNavItem").each(function () {
            $(this).removeClass("selected");
            var mstNavNavItemFlyout = $(this).parent().find(".mstNavNavItemFlyout");
            $(mstNavNavItemFlyout).css({ "z-index": "994" }).slideUp(200);
        });
        //    window.setTimeout(function () { semaphor = true; }, 300);
        //}
    })
        .on("click", ".oneMscomNavV3 .mstNavNavItem", function (event) {
            //if (semaphor) {
            $(allFlyouts).css({ "z-index": "994" }).slideUp(200);
            if ($(this).hasClass("selected")) { $(this).removeClass("selected"); event.preventDefault(); return true; }
            menuTabs.removeClass("selected");
            var mstNavNavItemFlyout = $(this).find(".mstNavNavItemFlyout");
            if (mstNavNavItemFlyout.length > 0) {
                event.preventDefault();
                $(this).toggleClass("selected");
                if ($(this).hasClass("selected")) {
                    $(mstNavNavItemFlyout).css({ "z-index": "995" }).stop(true, true).slideDown(800);
                } else {
                    $(mstNavNavItemFlyout).css({ "z-index": "994" }).stop(true, true).slideUp(200);
                }
                event.stopPropagation();

                //    }
                //} else {
                //    event.preventDefault();
            }
        })
        .on("click", ".mstNavNavItemFlyout", function (event) {
            event.stopImmediatePropagation();
        });



    var adjustNavFlyoutWidth = function () {
        var windowWidth = $(window).width();
        var pageWidth = $(".stage").width();
        var minus = windowWidth - pageWidth;

        if (minus > 0) {
            var padding = minus / 2;

            $(allFlyouts).css({
                "left": "-" + padding + "px",
                "width": pageWidth + "px",
                "padding-left": padding + "px",
                "padding-right": padding + "px"
            });
        }
    }

    adjustNavFlyoutWidth();

    $(window).resize(function () {
        adjustNavFlyoutWidth();
    });
});
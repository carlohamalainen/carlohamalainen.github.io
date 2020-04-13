$(function () {
    $('.epgPsSocialShareing .icon').hover(function () {

        $(".epgPsSocialShareing .icon .text").hide();
        $(this).find(".text").show();
        $(".epgPsSocialShareing").css("overflow", "visible");
        $(this).css("padding-right", ($(this).find(".text").width() + 5) + "px");
    }, function () {

        $(".epgPsSocialShareing .icon .text").show();
        $(".epgPsSocialShareing").css("overflow", "hidden");
        $(this).css("padding-right", "0px");
    });

    try {
        meteor.sharing.configure('9081c086-c500-4e70-9391-dbe2dea191c0');

        var current_href = $(location).attr('href');
        var current_title = $(document).attr('title');

        $(".epgSocialWidget-root-facebook").on("click", function () {
            this.href = meteor.sharing.href('Facebook', { 'url': current_href, 'title': current_title });
        });

        $(".epgSocialWidget-root-LinkedIn").on("click", function () {
            this.href = meteor.sharing.href('LinkedIn', { 'url': current_href, 'title': current_title });
        });

        $(".epgSocialWidget-root-twitter").on("click", function () {
            this.href = meteor.sharing.href('Twitter', { 'url': current_href, 'title': current_title });
        });


        $(".epgSocialWidget-root-email").on("click", function () {
            this.href = meteor.sharing.href('Email', { 'title': current_title, 'desc': current_href });
        });
    } catch (exc) { }


});



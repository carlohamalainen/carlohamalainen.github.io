// Place your application-specific JavaScript functions and classes here
// This file is automatically included by javascript_include_tag :defaults

$(document).ready(function(){
	if (navigator.userAgent.match(/like Mac OS X/i)) {
		if ($.cookie("hide_ios_banner") != "true") {
			$('#iosBanner').css('display', 'block');
			$('#hide_ios_link').click(function() {
				$('#iosBanner').css('display', 'none');
				$.cookie("hide_ios_banner", "true", {expires: 30, path: '/'});
			});
		}
    }
});	
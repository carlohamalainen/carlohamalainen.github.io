$(document).ready(function(){
	$('.coloredLink').each(function(index) {
		$(this).wrapInner('<span />');
		$(this).addClass('dynamicColoredLink');
	});
});
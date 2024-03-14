
$.ajax({
    url: document.location.protocol + '//munchkin.marketo.net/munchkin.js',
    dataType: 'script',
    cache: true,
    success: function() {
        Munchkin.init('112-YPQ-597');
    }
});


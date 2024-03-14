$(function () {
  if (typeof epgOpenModal === "undefined") {
    var scriptTag = document.createElement('script');
    scriptTag.type = 'text/javascript';
    scriptTag.src = '/global/enterprise/RenderingAssets/components/epgModal/epgModal.js';
    $("head").append(scriptTag);
    $("head").append("<link type='text/css' rel='stylesheet' href='/global/enterprise/RenderingAssets/components/epgModal/epgModal.css' />");
  }

  var allVideos = [];

  var videoObj = function (target, type, autoPlay) {
    this.type = type;
    switch (this.type) {
      case 'mp4':
        this.type = 'video/mp4';
        break;
      case 'webm':
        this.type = 'video/webm';
        break;
      case 'ogg':
      case 'ogv':
        this.type = 'video/ogg'
        break;
      default:
        break;
    }
    if (autoPlay !== false) {
      this.autoPlay = ' autoplay';
    } else {
      this.autoPlay = '';
    }
    this.poster = '';
    this.captions = [];
    this.src = target.attr('href');

    if (this.type === 'YouTube') {
      //Turn various YouTube URLs into 'embed' URLs
      this.src = this.src.replace(/(?:https?:)?(?:\/\/)?(?:www\.)?(?:youtube\.com|youtu\.be)\/(?:embed\/)?(?:watch\?v=)?(.+)/g, '//www.youtube.com/embed/$1');
    } else {
      var thisVidObg = this;
      target.children('input.popuPoster').each(function () {
        thisVidObg.poster = $(this).data('imgsrc');
      });
      target.children('input.popuCaption').each(function () {
        var caption = $(this);
        thisVidObg.captions.push({ src: caption.data('src'), lang: caption.data('lang'), langAbbr: caption.data('langabbr'), defaultCaption: caption.data('defaultcaption') });
      });
    }

    if (this.captions.length > 0 && typeof videojs === "undefined") {
      //Load video.js player for captions
      var scriptTag = document.createElement('script');
      scriptTag.type = 'text/javascript';
      scriptTag.src = '/global/enterprise/RenderingAssets/js/video-js/video.js';
      $("head").append(scriptTag);
      $("head").append("<link type='text/css' rel='stylesheet' href='/global/enterprise/RenderingAssets/js/video-js/video-js.css' />");
    }
  }

  $.fn.popuVideoModalWindow = function (/*autoplay boolean*/) {
    var target = $(this);
    var args = arguments;
    target.each(function () {
      var targetInstance = $(this);
      if (targetInstance.is("a")) {
        var autoPlay = true;
        if (args.length > 0) {
          for (var i = 0; i < args.length; i++) {
            if (typeof args[i] === "boolean") {
              autoPlay = args[i];
            }
          }
        }
        var href = targetInstance.attr('href');
        if (href.indexOf('youtu') !== -1) {
          allVideos.push(new videoObj(targetInstance, 'YouTube', autoPlay));
          targetInstance.attr('href', '#').on('click', epgPopupVideo).data('vidNum', (allVideos.length - 1));
        } else {
          var extensionIdx = href.lastIndexOf('.') + 1;
          var extension = href.substr(extensionIdx, href.length);
          if ((extension === 'mp4') || (extension === 'ogv') || (extension === 'ogg') || (extension === 'webm') || (extension === 'wmv') || (extension === 'wmx') || (extension === 'ism')) {
            allVideos.push(new videoObj(targetInstance, extension, autoPlay));
            targetInstance.attr('href', '#').on('click', epgPopupVideo).data('vidNum', (allVideos.length - 1));
           
          }
        }
      } else { }
    });
  };

  var epgPopupVideo = function (event) {
    if (typeof window.popupVideo !== 'undefined') {
      window.popupVideo.dispose();
      window.popupVideo = undefined;
    }
    this.vidJSLoadTryCount = 0;
    this.initVidJS = function () {
      if (typeof videojs !== 'undefined') {
        window.popupVideo = videojs('epgPopupVideo');
        //epgPopupVideo is now a parent div to the original video
        $('#epgPopupVideo').addClass('vjs-default-skin video-js').css({ 'width': '100%', 'height': '100%', 'background-color': '#000', 'position': 'absolute', 'top': '0px', 'left': '0px', 'font-size': '13px' });
        $('#epgPopupVideo .vjs-subtitles-button-shown:before').css({ 'content': "'\e008' !important" });
        $('#epgPopupVideo .vjs-text-track-display').css({ 'left': '50%', 'right': 'auto' });
        $('#epgPopupVideo .vjs-big-play-button').css({ 'display': 'none' });
        $('#epgPopupVideo .vjs-menu-content > li').css({ 'text-transform': 'capitalize' });
        //.vjs-text-tracks are not in the DOM until captions are turned on, so apply CSS changes to head.
        $('head').append('<style>#epgPopupVideo .vjs-text-track{left:-50%; position:relative; padding:0 0.3em;}</style>');

        var csIntervalCount = 0;
        var checkStarted = setInterval(function () {
          if ($('#epgPopupVideo').hasClass('vjs-playing')) {
            window.popupVideo.pause().play();
            clearInterval(checkStarted);
          } else {
            csIntervalCount++;
            if (csIntervalCount >= 100) {
              clearInterval(checkStarted);
            }
          }
        }, 100);
      } else {
        if (this.vidJSLoadTryCount < 100) {
          this.vidJSLoadTryCount++;
          setTimeout(this.initVidJS, 100);
        } else {
          //Stop trying to init video.js plugin if file hasn't loaded after 10 seconds 
        }
      }
    }

    event.preventDefault ? event.preventDefault() : event.returnValue = false;
    target = (event.currentTarget) ? event.currentTarget : event.srcElement;
    videoInstance = allVideos[$(target).data('vidNum')];
    if ($(window).width() <= 600) {
      var win = window.open(videoInstance.src, '_blank');
      win.focus();
    } else {
      if (videoInstance.type === 'YouTube') {
        var htmlString = "<div id='epgPopupYoutubeCont' style='width:100%; height:auto; max-width:800px; max-height:100%; position:relative;'>" +
            "<div class='epgPopupYTHeight' style='width:100%; padding-bottom:56.25%;'>" +
              "<iframe src='" + videoInstance.src + "' style='position:absolute; height:100%; width:100%; top:0px; left:0px;' frameborder='0' allowfullscreen></iframe>" +
            "</div>" +
          "</div>";
      } else {
        var htmlString = "<div style='width:100%; height:auto; max-width:800px; max-height:100%; position:relative; overflow:hidden; text-align:left;'>" +
          "<div style='padding-bottom:56.25%; position:relative;'>" +
          "<div style='width:100%; height:100%; text-align:left; position:absolute; background-color:#000000;'>" +
          "</div>" +
          "</div>";
        if (videoInstance.type !== 'wmv') {
          htmlString += "<video id='epgPopupVideo' style='width:100%; height:100%; background-color:#000; position:absolute; top:0px; left:0px;' controls" + videoInstance.autoPlay + " poster='" + videoInstance.poster + "'>" +
  	      "<source src='" + videoInstance.src + "' ";
          if ((videoInstance.type === 'video/mp4') || (videoInstance.type === 'video/webm') || (videoInstance.type === 'video/ogg')) {
            htmlString += "type='" + videoInstance.type + "' "
          }
          htmlString += "/>";

          for (var i = 0; i < videoInstance.captions.length; i++) {
            htmlString += "<track src='" + videoInstance.captions[i].src + "' kind='subtitles' srclang='" + videoInstance.captions[i].langAbbr + "' label='" + videoInstance.captions[i].lang;
            if (videoInstance.captions[i].defaultCaption === true) {
              htmlString += "' default />";
            } else {
              htmlString += "' />";
            }
          }

        }
        var silverlightAutoplay = videoInstance.autoPlay;
        if (videoInstance.autoPlay === " autoplay") {
          silverlightAutoplay = true;
        }
        htmlString += "<object class='pf-silverlight' style='position:absolute; width:100%; height:100%; top:0px; left:0px;' data='data:application/x-silverlight-2,' type='application/x-silverlight-2' data-poster='" + videoInstance.poster + "'>" +
  		    "<param name='source' value='/global/enterprise/RichMedia/smooth/SmoothStreamingPlayer.xap'/>" +
  		    "<param name='minRuntimeVersion' value='4.0.50826.0' />" +
  		    "<param name='autoUpgrade' value='true' />" +
  		    "<param name='enableGPUAcceleration' value='true' />" +
  		    "<param name='windowless' value='true' />" +
          "<param name='allowfullscreen' value='true' />" +
  		    "<param name='InitParams' value='scriptablename=Player,autoplay=" + silverlightAutoplay;

          //Consistent the window.location.protocol == videoInstance.src protocol,so when popup .wmv the silverlight can play the video when the address start with https://
        if (videoInstance.type == 'wmv') {
            var currProtocol = window.location.protocol;
            if (videoInstance.src.toUpperCase().indexOf('HTTP:') == 0) {
                videoInstance.src = videoInstance.src.replace(videoInstance.src.substr(0, 5), currProtocol);
            } else if (videoInstance.src.toUpperCase().indexOf('HTTPS:') == 0) {
                videoInstance.src = videoInstance.src.replace(videoInstance.src.substr(0, 6), currProtocol);
            } else if (videoInstance.src.toUpperCase().indexOf('//') == 0) {
                videoInstance.src = currProtocol + videoInstance.src;
            } else {
                videoInstance.src = currProtocol + "//"+ videoInstance.src;
            }
        }

        var ismIdx = videoInstance.src.lastIndexOf('.ism');
        if (ismIdx > 0) {
          //Make sure '/manifest' is at the end of the URL for .ism files, otherwise add it
          var manifestIdx = videoInstance.src.indexOf('/manifest');
          if ((manifestIdx > 0) && (manifestIdx + 9 === videoInstance.src.length)) {
            htmlString += ", mediaurl=" + videoInstance.src;
          } else if (ismIdx + 4 === videoInstance.src.length) {
            htmlString += ", mediaurl=" + videoInstance.src + "/manifest";
          } else {
            console.log('Video extension error');
          }
        } else {
          htmlString += ", deliverymethod=Progressive Download, mediaurl=" + videoInstance.src;
        }
        htmlString += "' />";

        /* Place Flash fallback here */

        htmlString += "</object>";
        if (videoInstance.type !== 'wmv') {
          htmlString += "</video>";
        }
        htmlString += "</div>";
      }
      epgOpenModal(htmlString);

      if (videoInstance.captions.length > 0) {
        this.initVidJS();
      }
    }
  };
});
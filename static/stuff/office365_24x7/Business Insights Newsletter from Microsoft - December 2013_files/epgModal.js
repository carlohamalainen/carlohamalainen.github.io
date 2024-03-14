var epgModalCreated = false;
var currentObjPercentW = "0";
var currentObjPercentH = "0";
var currentObjPercentMaxW = "0";
var currentObjPercentMaxH = "0";
var widthLimited = false;
var heightLimited = false;
var dimsAlertDisplayed = false;
var widthAdjusted = false;
var scrollBarActivated = false;

function epgOpenModal(htmlString) {
  $('#epgModal > .epgModalBorderProtector').css({'line-height':($(window).height() - 100) + 'px'});
  if (epgModalCreated === false) {
    createEPGModal();
  }
  var clearFixVert = "<div class='clearFixVert'>Vertical Clear-Fix</div>"
  $('#epgModal > .epgModalBorderProtector').html(htmlString + clearFixVert);
  $('#epgModal').fadeIn(function(){
    var contentObj = $('#epgModal > .epgModalBorderProtector').children().first();
    contentObj.on("click", function (event) {
      event.stopPropagation();
    });
    contentObj.resize(function(){
      checkCloseButtonSize(contentObj);
    });
    checkCloseButtonSize(contentObj);
    $("#epgModal .epgModalCloseButton").fadeIn();

    $(document).keyup(checkESCKey);
  });
}

function checkCloseButtonSize(contentObj){
  var windowW = $(window).width();
  var windowH = $(window).height();
  var setSmallButton = false;
  var moveCloseInterval = setInterval(function(){
    if((contentObj.outerWidth() > 150)&&(contentObj.outerHeight() > 150)){
            
      if (contentObj.outerHeight() > $('.epgModalBorderProtector').outerHeight()) {
        var centeringHeight = $('.epgModalBorderProtector').outerHeight();
        var scrollBarActivated = true;
      } else {
        var centeringHeight = contentObj.outerHeight();
        var scrollBarActivated = false;
      }
      if (contentObj.outerWidth() > $('.epgModalBorderProtector').outerWidth()) {
        var centeringWidth = $('.epgModalBorderProtector').outerWidth();
      } else {
        var centeringWidth = contentObj.outerWidth();
      }
      
      if(contentObj.outerWidth() >= windowW - 175){
        setSmallButton = true;
        if(contentObj.outerWidth() >= windowW - 90){
          widthLimited = true;
        }else if((scrollBarActivated === true)&&(contentObj.outerWidth() >= windowW - 105)){
          widthLimited = true;
        }else{
          widthLimited = false;
        }
      }
      if(contentObj.outerHeight() >= windowH - 175){
        setSmallButton = true;
      }

      if(setSmallButton === false){
        positionCloseButton(centeringHeight, centeringWidth);
      }else{
        positionSmallCloseButton(centeringHeight, centeringWidth);
      }
      clearInterval(moveCloseInterval);
    }
  }, 10);
}

function positionCloseButton(centeringHeight, centeringWidth) {
  var closeOffsetX = (centeringWidth / 2) + 25;
  var closeOffsetY = 0 - (centeringHeight / 2) - 50;
  $("#epgModal .epgModalCloseButton").css({ 'margin': closeOffsetY + 'px 0 0 ' + closeOffsetX + 'px' });
  $("#epgModal .epgModalCloseButton").removeClass('epgModalSmallCloseButton');
}

function positionSmallCloseButton(centeringHeight, centeringWidth) {
  if(widthLimited === true){
    offsetX = -23;
    offsetY = 43;
  }else{
    offsetX = 10;
    offsetY = 40;
  }
  var closeOffsetX = (centeringWidth / 2) + offsetX;
  var closeOffsetY = 0 - (centeringHeight / 2) - offsetY;
  $("#epgModal .epgModalCloseButton").css({'margin': closeOffsetY + 'px 0 0 ' + closeOffsetX + 'px' });
  $("#epgModal .epgModalCloseButton").addClass('epgModalSmallCloseButton');
}

function checkESCKey(e){
  if (e.keyCode == 27) { epgCloseModal(); }
}

function epgCloseModal() {
  if (typeof window.popupVideo !== 'undefined') {
    window.popupVideo.dispose();
    window.popupVideo = undefined;
  }
  $('#epgModal > .epgModalBorderProtector').html('');
  $("#epgModal .epgModalCloseButton").css({ 'margin': '-25px 0 0 -24px', 'display':'none' });
  $('#epgModal').fadeOut();
  $(document).unbind("keyup", checkESCKey);
}

function createEPGModal() {
  if($('#epgModal').length === 0){
    var html = "<div id='epgModal'>" +
                  "<div class='epgModalBG' onclick='javascript:epgCloseModal();'></div>" +
                  "<div class='epgModalCloseButton' onclick='javascript:epgCloseModal();'>"+
                    "<img src='/global/enterprise/PublishingImages/epgModalClose.png' />" +
                  "</div>" +
                  "<div class='epgModalBorderProtector' onclick='javascript:epgCloseModal();'></div>"+
                "</div>";
    $('body').append(html);
    epgModalCreated = true;
  }
}

$(document).ready(function () {
  createEPGModal();
  $(window).resize(function(){
    $('#epgModal > .epgModalBorderProtector').css({'line-height':($(window).height() - 100) + 'px'});
    var contentObj = $('#epgModal > .epgModalBorderProtector').children().first();
    checkCloseButtonSize(contentObj);
  });
}); 
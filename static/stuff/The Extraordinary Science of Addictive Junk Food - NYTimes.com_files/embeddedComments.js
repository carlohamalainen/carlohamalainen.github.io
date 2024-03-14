/**
 * embeddedComments.js
 * $Id: embeddedComments.js 122984 2013-03-19 14:49:27Z jessica.kosturko $
 */

function EmbeddedComments(jQuery, objName) {
    'use strict';
    var $ = jQuery;

    var pageUrl, $commentsContainer, refreshTimeoutVar;
    var wrapper = '/svc/community/V3/requestHandler';
    var $document = $(window.document);
    var $window = $(window);
    var state = {};
    var config = {};
    var WTobj = {};

    var initializeComments = function (args) {
        var communityAssetTaxonomy, taxonomyBuilder, CG, hdl, slug;
        var location = window.location;
        var permidParts = /^#permid=(\d+(:\d+)*)$/.exec(location.hash) || /[?&]permid=(\d+(:\d+)*)/.exec(location.search);

        pageUrl = args.pageUrl || location.href;
        pageUrl = pageUrl.replace(/[?#].*/, '');

        //set default values
        config = commentsConfig[args.config] || commentsConfig['default'];
        config.autoRefresh = (typeof args.autoRefresh !== 'undefined') ? args.autoRefresh : config.autoRefresh;
        config.autoRefreshTime = (typeof args.autoRefreshTime !== 'undefined') ? args.autoRefreshTime : config.autoRefreshTime;
        config.showPeek = (typeof args.showPeek !== 'undefined') ? args.showPeek : config.showPeek;

        $commentsContainer = (args.commentsContainer) ? $('#' + args.commentsContainer) : $('#commentsContainer');

        //Load the default settings
        state = {
            displayStyle: 'normal',
            assetTaxonomy: '',
            commentState: 'notLoaded',
            loaded: false,
            commentCounts: {
                all: 0,
                readerpicks: 0,
                nytpicks: 0,
                nytreplies: 0
            },
            currentTabCount: 0,
            offset: 0,
            canSubmit: true,
            commentFormLocation: 'notyet',
            replyFormLocation: null,
            currentTab: null,
            currentSort: config.defaultSort, //1 is newest first, 0 is oldest first
            userData: null,
            userID: 0,
            userEmail: null,
            lastPostedInfo: {},
            isScrollEventLoaded: false,
            permid: null,
            sharetitle: $("meta[name=hdl]").attr("content") || document.title,
            commentNotify: 0,
            gotoCommentFormNow: false,
            permidURL: location.href.replace(/[?#].*/, '')  + '?comments',
            WTstats: {
                comments: {},
                replies: {},
                tablist: {}
            }
        };

        if (permidParts && permidParts[1]) {
            state.displayStyle = 'permalink';
            config.defaultTab = 'permalink';
            state.permid = permidParts[1];
            window.scrollTo(0, $commentsContainer.offset().top - 15);
        }

        if (/^#postcomment$/.test(location.hash)) {
            state.gotoCommentFormNow = true;
            window.scrollTo(0, $commentsContainer.offset().top - 15);
        }

        //Create the taxonomy from either meta tags or a supplied argument - Provides
        if (!args.assetTaxonomy) {
            communityAssetTaxonomy = $("meta[name=communityAssetTaxonomy]").attr("content");
            if (communityAssetTaxonomy && communityAssetTaxonomy !== '') {
                state.assetTaxonomy = communityAssetTaxonomy;
            } else {
                taxonomyBuilder = [];

                CG = $("meta[name=CG]").attr("content");
                hdl = $("meta[name=hdl]").attr("content");

                if (CG && CG !== '') {
                    taxonomyBuilder.push(CG);
                }

                if (hdl && hdl !== '' && typeof hdl === 'string') {
                    hdl =  hdl.replace(/\&\%\?/g, "");
                    taxonomyBuilder.push(hdl);
                }

                state.assetTaxonomy = taxonomyBuilder.join('/');

                slug = $("meta[name=slug]").attr("content");
                if (slug && slug !== '') {
                    state.assetTaxonomy += ' (' + slug + ')';
                }

            }
        } else {
            state.assetTaxonomy = args.assetTaxonomy;
        }

        //Gets basic information about visitor
        loadContent({'cmd': 'GetBasicInfo'}, setBasicInfo);
    };

    //Provides context for selectors so IDs can continue to be used safely.
    var $container = function (selector) {
        return $commentsContainer.find(selector);
    };

    var setBasicInfo = function (response) {
        var jshost = NYTD.Hosts.jsHost || state.www;

        if (response) {
            state.userID = (response.userID !== '') ? response.userID : 0;
            state.userEmail = response.userEmail;
            state.myaccounturl = response.myaccounturl;
            state.timespeople = response.tphost;
            state.commentNotify = response.comment_notify;
            state.www = response.wwwhost;

            if (!window.JSON) {
                loadJSFile(jshost + '/js/app/lib/json/json2-min.js');
            }

            if (!window.NYTD.shareTools) {
                loadJSFile(jshost + '/js/common/sharetools/2.0/shareTools.js');
            }

            //AJAX request for comments header
            loadContent({
                cmd: 'GetCommentSummary',
                getdata: {
                    path: pageUrl
                }
            }, drawCommentModule);
        }
    };

    var drawCommentModule = function (cmt) {
        var $headerContainer;
        var $articleDateline = $('#article .dateline');
        var results = cmt.results;
        var data = {};

        if (results) {
            //populate template data
            data.cCount = results.totalCommentsFound;
            data.commentQuestion = results.commentQuestion;

            //configure comment settings
            state.userData = results.userData || {};
            state.userData.hasProfile = 0;
            state.canSubmit = !!results.canSubmit;
            state.commentCounts = {
                all: results.totalParentCommentsFound,
                nytpicks: results.totalEditorsSelectionFound,
                readerpicks: results.totalRecommendationsFound,
                nytreplies: results.totalReporterReplyCommentsFound
            };

            if (state.userData.displayName && state.userData.location) {
                state.userData.hasProfile = 1;
            }

            if (!state.userData.sharing) {
                state.userData.sharing = 0;
            }

            if (results.api_timestamp > 0) {
                state.currentTime = parseInt(results.api_timestamp, 10);
            }

            if (results.sortBy && state.displayStyle !== 'permalink') {
                switch (results.sortBy) {
                case 'comment-list-sort-approvedate':
                    config.defaultTab = 'all';
                    config.defaultSort = 0;
                    state.currentSort = 0;
                    break;
                case 'comment-list-sort-approvedate-desc':
                    config.defaultTab = 'all';
                    config.defaultSort = 1;
                    state.currentSort = 1;
                    break;
                case 'comment-list-sort-recommended':
                    config.defaultTab = 'readerpicks';
                    break;
                case 'comment-list-sort-editors':
                    config.defaultTab = 'nytpicks';
                    break;
                case 'comment-list-sort-replies':
                    config.defaultTab = 'nytreplies';
                    break;
                }
            }

            //add template to the screen
            $commentsContainer.append(config.commentsModuleDiv());

            //update template header
            $headerContainer = $container('#commentsHeaderData');
            $headerContainer.append(config.commentsHeaderData(data));

            if ($.browser.msie && $.browser.version < 7) {
                $headerContainer.append(config.ie6Message());
            } else {
                drawNavBar();
                drawCommentDisplay();
                if (state.canSubmit === true && results.totalCommentsFound > 0) {
                    $container('#toggleFormButton').on('click', showCommentForm);
                }
            }

            //Dateline comment tag with click tracking
            if ($articleDateline.length > 0) {
                $articleDateline
                    .append(config.bylineCommentMarker(data))
                    .on('click', '.commentCountLink', {'WT.z_aca': 'Coms-Byline'}, immediateTrack);
            }

            //Add verified modal to the page
            createVerifiedModal();

            //Add flaged modal to the page
            createFlagCommentModal();

            //Add photo upload modal to the page
            createAvatarUploadModal();

            //let everyone know the comments module is complete
            $document.trigger('NYTD:CommentSetupComplete');
        }
    };

    var drawNavBar = function () {
        var data = {
            style: state.displayStyle,
            readerpicksct: state.commentCounts.readerpicks,
            nytpicksct: state.commentCounts.nytpicks,
            nytrepliesct: state.commentCounts.nytreplies,
            canSubmit: state.canSubmit,
            sortText: (state.currentSort === 0) ? 'Oldest' : 'Newest'
        };

        $container("#commentsNavBar")
            .replaceWith(config.navbar(data));

        $container("#commentsNavBar")
            .on('click', '.tabsContainer li, .tabsContainer p', showCommentsViaEvent)
            .find('li:last-child')
            .addClass('lastItem');
    };

    var drawCommentDisplay = function () {
        if (state.loaded === false) {
            $container("#commentsDisplayContainer").html(config.commentDisplay());
            setupLoadEvent();
        }
    };

    var setupLoadEvent = function () {
        if (!state.isScrollEventLoaded) {
            state.isScrollEventLoaded = true;

            if (checkToLoadDiv()) {
                $document.on('NYTD:CommentSetupComplete', announceSetupComplete);
            } else {
                $window.on('scroll', scrollToLoadHandler);
            }
        }
    };

    var announceSetupComplete = function () {
        webTrendsSetup();
        showComments(config.defaultTab, false);
        $document.trigger('NYTD:EmbeddedCommentsStarted');

        if (config.autoRefresh) {
            config.autoRefreshTime = (config.autoRefreshTime < 60000) ? 60000 : config.autoRefreshTime;
            startAutoRefresh();
        }
    };

    var scrollToLoadHandler = function () {
        if (state.commentState === 'notLoaded' && checkToLoadDiv()) {
            $window.off('scroll', scrollToLoadHandler);
            announceSetupComplete();
        }
    };

    var showCommentsViaEvent = function (e) {
        var displayType, $toggleFormButton, action;

        removePeek();
        e.preventDefault();

        $toggleFormButton = $container('#toggleFormButton');
        displayType = $(this).attr('tabname');
        state.WTstats.tablist[displayType] = 1;

        switch (displayType) {
            case 'all':
            action = 'comments-tab-all-click';
            break;
            case 'nytpicks':
            action = 'comments-tab-nyt-picks-click';
            break;
            case 'nytreplies':
            action = 'comments-tab-nyt-replies-click';
            break;
            case 'readerpicks':
            action = 'comments-tab-readers-picks-click';
            break;
            case 'permalink':
            action = 'comments-permalink-click';
            state.currentTab = 'permalink';
            state.displayStyle = 'normal';
            window.location.hash = '#comments';
            drawNavBar();
            displayType = 'all';
            break;
        }

        $toggleFormButton
            .off('click')
            .on('click', showCommentForm);

        sendEventTracker(action);

        showComments(displayType, false);
        $container('#commentFormTop').empty();
        hideMe($container('#commentFormBottom').empty());
        state.commentFormLocation = "bottom";
        drawCommentForm();
    };

    var showComments = function (displayType, sortToggle) {
        var tab, tabId, tabLength;
        var $toggleControl = $container('#sortToggle');
        var tabWeAreLookingFor = '#commentsNav';
        var tabList = ['#commentsNavAll', '#commentsNavReaderPicks', '#commentsNavNYTPicks', '#commentsNavNYTReplies'];

        if (!displayType || displayType !== state.currentTab || sortToggle) {

            state.loaded = false;
            state.currentTab = (!state.currentTab) ? config.defaultTab : displayType;
            state.currentTabCount = state.commentCounts[state.currentTab];

            switch (displayType) {
            case 'readerpicks':
                tabWeAreLookingFor = tabWeAreLookingFor + 'ReaderPicks';
                break;
            case 'nytpicks':
                tabWeAreLookingFor = tabWeAreLookingFor + 'NYTPicks';
                break;
            case 'nytreplies':
                tabWeAreLookingFor = tabWeAreLookingFor + 'NYTReplies';
                break;
            case 'all':
                tabWeAreLookingFor = tabWeAreLookingFor + 'All';
                break;
            default:
                tabWeAreLookingFor = tabWeAreLookingFor + 'All';
                break;
            }

            for (tabId = 0, tabLength = tabList.length; tabId < tabLength; tabId += 1) {
                tab = tabList[tabId];
                if (tab === tabWeAreLookingFor) {
                    $container(tab).addClass('selected');
                    $container(tabList[tabId - 1]).addClass('precedesSelected');
                } else {
                    if ($container(tab).hasClass('selected')) {
                        $container(tab).removeClass('selected');
                    }
                    if ($container(tab).hasClass('precedesSelected')) {
                        $container(tab).removeClass('precedesSelected');
                    }
                }
            }

            $toggleControl.off('click');
            if (displayType === 'all') {
                $toggleControl
                    .show()
                    .on('click', handleSortClick);
                if (sortToggle === false) {
                    state.currentSort = config.defaultSort;
                }
            } else {
                $toggleControl.hide();
            }

            state.offset = 0;
            loadComments(displayType);
        }
    };

    var checkToLoadDiv = function () {
        if (/^#postcomment$/.test(window.location.hash)) {
            state.gotoCommentFormNow = true;
            window.scrollTo(0, $commentsContainer.offset().top - 15);
            return true;
        
        } else if (state.gotoCommentFormNow || state.displayStyle === 'permalink' || $commentsContainer.offset().top < $window.height() + $window.scrollTop()) {
            return true;
        
        } else {
            return false;
        }
    };

    var flagComment = function (e) {
        var postdata, i, arrLen;
        var reasons = [];
        var commentID = e.data.commentID;
        var $checkboxes = $container('.commentFlagModal .checkbox');

        for (i = 0, arrLen = $checkboxes.length; i < arrLen; i += 1) {
            if ($checkboxes[i].checked) {
                reasons.push($checkboxes[i].value);
            }
        }

        postdata = {
            path: pageUrl,
            commentID: commentID,
            userID: state.userID,
            commentLabels: reasons.join(',')
        };

        loadContent({
            postdata: postdata,
            method: 'post',
            cmd: 'FlagComment'
        }, function () {
            toggleFlag(commentID);
        });

        sendEventTracker('comments-flag');
    };

    var toggleFlag = function (commentID) {
        $container('#comment' + commentID + ' .commentFlagContainer').first().html(config.commentFlagged());
    };

    var handleSortClick = function () {
        sortComments();
    };

    var sortComments = function () {
        var $toggleControl = $container('#sortToggle');

        state.currentSort = (state.currentSort + 1) % 2;
        state.loaded = false;

        if (state.currentSort === 1) {
            $toggleControl.html('Newest');
        } else {
            $toggleControl.html('Oldest');
        }

        showComments(state.currentTab, true);

        //If the write comment module hasn't displayed yet, we need to show it.
        if ($container('#commentFormControl').length === 0) {
            drawCommentForm();
            state.commentFormLocation = "bottom";
        }
    };

    var showCommentForm = function (e) {
        var $formControl, $formTop, $formBottom;

        removePeek();
        
        $formControl = $container('#commentFormControl');
        $formTop = $container('#commentFormTop');
        $formBottom = $container('#commentFormBottom');

        if (state.replyFormLocation) {
            $(state.replyFormLocation).remove();
            state.replyFormLocation = null;
        }

        if (state.commentFormLocation !== 'top') {
            showMe($formTop.prepend($formControl), true);
            hideMe($formBottom.empty());
            state.commentFormLocation = 'top';
            $formControl.removeClass('hidden');
            if ($formControl.next().hasClass('commentConfirmation')) {
                $formControl.find('.doubleRuleDivider').addClass('hidden');
            }
        }

        // If the user clicked on 'Write a Comment'
        if (e && e.target) {
            sendEventTracker('comments-write-new-click');
        }
    };

    var hideCommentForm = function () {
        removePeek();
        $container('#commentFormControl').addClass('hidden');
        state.commentFormLocation = 'hidden';
    };

    var clearCommentForm = function () {
        $container('#commentTextarea, #commenterFullNameInput, #commenterLocationInput').val('');
        hideMe($container('#submitLoader'));
    };

    var removePeek = function () {
        var $footer = $container('#commentsFooter');

        if (state.commentState === 'peek') {
            state.commentState = 'loaded';

            $container('#commentsList')
                .removeClass('commentsListPeek')
                .attr('style', '');

            $footer
                .removeClass('commentsFooterPeek')
                .attr('style', '');

            drawCommentForm();

            if (state.offset >= state.currentTabCount) {
                $footer.removeClass('singleRule').empty();
                $container('#commentsReadMoreToggle').off('click');
            }
        }
    };

    /*
     * Using the Overlay Modal library, create a trusted modal
     */
    var createVerifiedModal = function () {
        var closeTimeout;
        var trustedModal = NYTD.UI.OverlayModal({
            uniqueId: 'commentsTrustedModal',
            modalTitle: '',
            positionType: "Above",
            bind: '.trustedModal',
            hideCloseText: true,
            overlayBackground: 'None',
            width: 265,
            modalHTML: config.dialogBoxTrusted(),
            hoverCallback: function (e) {
                if (e.type === 'mouseenter') {
                    window.clearTimeout(closeTimeout);
                    $container('#commentsTrustedModalContainer .moduleHeaderBd').html($(this).closest('span').data('modalTitle'));
                    trustedModal.open.call(this, e);
                } else {
                    closeTimeout = setTimeout(trustedModal.close, 500);
                }
            }
        });

        //add modal to page (inside the comments module)
        trustedModal.addToPage($commentsContainer.find('#commentsModule'));

        //keep the box from closing when the mouse is over the modal
        $container('#commentsTrustedModalContainer').on('hover', '.nytModal', function (e) {
            if (e.type === 'mouseenter') {
                window.clearTimeout(closeTimeout);
            } else {
                closeTimeout = window.setTimeout(trustedModal.close, 500);
            }
        });
    };

    var createFlagCommentModal = function () {
        var closeTimeout, commentID;
        var flagModal = NYTD.UI.OverlayModal({
            uniqueId: 'commentFlagModal',
            modalTitle: 'Report Inappropriate Comment',
            positionType: "Above",
            additionalClasses: 'dialogBox',
            bind: '.commentFlag',
            hideCloseText: true,
            overlayBackground: 'None',
            width: 300,
            modalHTML: config.commentFlaggedModal(),
            clickCallback: function () {
                commentID = $(this).data('commentID');
                
                //remove all checks
                $container('#commentFlagModalContainer .checkbox').removeAttr('checked');

                //on submit
                $container('#commentFlagModalContainer .flagCommentSubmit')
                    .on('click', { commentID: commentID}, flagComment)
                    .on('click', flagModal.close);
            },
            closeCallback: function () {
                $container('#commentFlagModalContainer .flagCommentSubmit')
                    .off('click', flagComment)
                    .off('click', flagModal.close);
            },
            hoverCallback: function (e) {
                if (e.type === 'mouseleave') {
                    closeTimeout = setTimeout(flagModal.close, 500);
                }
            }
        });

        //add modal to page (inside the comments module)
        flagModal.addToPage($commentsContainer.find('#commentsModule'));

        //keep the box from closing when the mouse is over the modal
        $container('#commentFlagModalContainer').on('hover', '.nytModal', function (e) {
            if (e.type === 'mouseenter') {
                window.clearTimeout(closeTimeout);
                $('#comment' + commentID).addClass('commentActive');
            } else {
                closeTimeout = window.setTimeout(flagModal.close, 200);
                $('#comment' + commentID).removeClass('commentActive');
            }
        });
    };

        var createAvatarUploadModal = function () {
        var closeTimeout, commentID, imageCropper, isUploaded, lastCoords;
        var $originals, $avatarUploadForm, $avatarCropForm, $avatarCropButton, $container, $spinner, $fileElement;
        var jsHost = NYTD.Hosts.jsHost || state.www;
        var avatarModal = NYTD.UI.OverlayModal({
            uniqueId: 'avatarUploadModal',
            modalTitle: 'Add or Edit Your Photo',
            positionType: "RightAbove",
            additionalClasses: 'dialogBox',
            bind: '.photoUpload img',
            hideCloseText: true,
            overlayBackground: 'None',
            width: 500,
            height: 420,
            modalHTML: config.avatarUploadModal(),
            openCallback: function() {

                $container.find('.editProfileMessage').show()
                    .end().find('.agreementMessage').hide();

                $avatarUploadForm.ajaxForm({
                    dataType:  'json',
                    complete: function(xhr) {
                        var response = (xhr) ? JSON.parse(xhr.responseText) : {};
                        
                        if (response.code === 200){

                            $avatarUploadForm.hide();
                            $avatarCropButton.show();

                            $container.find('.error, .editProfileMessage').hide()
                                .end().find('.agreementMessage').show();

                            $('.imageCropper').append('<img src="' + response.result + '" class="uploadedAvatar"/>').show();
                            loadCropper($('.uploadedAvatar'));

                            isUploaded = true;
                        }
                        else {
                            var msg;
                            if (response.result && response.result.ERROR) {
                                msg = response.result.ERROR;
                            }
                            showError(msg);
                            $fileElement.val('');
                        }
                        $spinner.hide();
                    },
                    error: function() {
                        showError('Unable to upload avatar at this time.  Please try again later.');
                        $spinner.hide();
                        $fileElement.val('');
                    },
                    timeout: 10000
                });

                $avatarCropForm.ajaxForm({
                    complete: function(xhr) {
                        var response = (xhr) ? JSON.parse(xhr.responseText) : {};
                        
                        if (response.code === 200){
                            $container.find('.error').hide();
                            refreshAvatar(response.result);
                        }
                        
                        $spinner.hide();
                        isUploaded = false;
                        avatarModal.close();
                    },
                    error: function() {
                        showError('Unable to crop your avatar at this time.  Please try again later.');
                        $spinner.hide();
                    }
                });

                $container.find('[name=profileImg]').on('change', function(e) {
                    var target = e.currentTarget;
                    if (target.files.length > 0) {
                        $spinner.show();
                        $avatarUploadForm.submit();
                    }
                });

            },
            closeCallback: function(e) {
                if (isUploaded) {
                    submitCrop(e, lastCoords);
                    isUploaded = false;
                }
                resetElements();
            }
        });

        var refreshAvatar = function(src) {
            src = src || $('.photoUpload').find('.replyAvatar').attr('src');

            $('.photoUpload').find('.replyAvatar').replaceWith('<img src="' + src + '" class="replyAvatar" />')
                .end().find('.replyLinkText').html('Change Photo');
            $container.find('.avatarPreviewImg').replaceWith('<img src="' + src + '" class="avatarPreviewImg" />');
        };

        var showError = function(message) {
            $container.find('.error').html(message || 'Could not update your photo at this time.').show();
        };

        var submitCrop = function(e, override) {
            var coords = override || imageCropper.crop();

            $spinner.show();

            $avatarCropForm.find('[name=x1]').attr('value', coords.x1)
                .end().find('[name=x2]').attr('value', coords.x2)
                .end().find('[name=y1]').attr('value', coords.y1)
                .end().find('[name=y2]').attr('value', coords.y2)
                .end().find('[name=width]').attr('value', coords.width)
                .end().find('[name=height]').attr('value', coords.height)
                .end().submit();
        };

        var resetElements = function() {
            $avatarUploadForm.replaceWith($originals.$avatarUploadForm.clone());
            $avatarCropForm.replaceWith($originals.$avatarCropForm.clone());

            $avatarUploadForm = $container.find('.avatarUploadForm');
            $avatarCropForm = $container.find('.avatarCropForm');
            $fileElement = $container.find('[name=profileImg]');

            $avatarCropButton.hide();
            $container.find('.error, .agreementMessage').hide()
                .end().find('.imageCropper').empty()
                .end().find('.editProfileMessage').show();
        };

        var loadCropper = function(img) {
            var $preview = $('.avatarPreview');

            imageCropper = img.imageCropper({
                constrain : true
            }).data('imageCropper');

            imageCropper.on('init dragend', function(e) {
                lastCoords = e;
            });

            $preview.empty().append(imageCropper.getPreviewObject());
        };

        if (typeof $.fn.ajaxForm === 'undefined') {
            loadJSFile(jsHost + '/js/app/lib/jquery/jquery.form.js');
        }
        if (typeof $.fn.imageCropper === 'undefined') {
            loadJSFile(jsHost + '/js/app/myprofile/jquery.imageCropper.js');
        }

        //add modal to page (inside the comments module)
        avatarModal.addToPage($commentsContainer.find('#commentsModule'));

        $container = $('.avatarUploadModal');
        $avatarUploadForm = $container.find('.avatarUploadForm');
        $avatarCropForm = $container.find('.avatarCropForm');
        $avatarCropButton = $container.find('.applyCrop').click(submitCrop).hide();
        $spinner = $container.find('.uploadSpinner').hide();
        $fileElement = $container.find('[name=profileImg]');

        $originals = {};
        $originals.$avatarUploadForm = $avatarUploadForm.clone();
        $originals.$avatarCropForm = $avatarCropForm.clone();

        $("<link/>", {href: NYTD.Hosts.cssHost + '/css/0.1/screen/common/modal/avatarModal.css', rel: 'stylesheet', type: 'text/css'}).prependTo('head');
        
        $(document).on('click', '.replyLinkText', function() {
            avatarModal.open();
        });
    };

    var drawCommentForm = function (commentID) {
        var data, commentForm, $regLink;
        var $formBottom = $container('#commentFormBottom');

        //If commentID === undefined then this is the form to add a new Comment
        //If commentID !== undefined then this is a reply to a comment.
        if (state.canSubmit === true) {

            data = {
                hasProfile: state.userData.hasProfile,
                displayName: removeHTML(state.userData.displayName),
                location: removeHTML(state.userData.location),
                title: state.userData.title,
                email: state.userEmail,
                trusted: state.userData.trusted,
                charCount: config.maxCommentSize,
                commentID: commentID,
                picURL: state.userData.picURL,
                myaccountfeedbackurl: state.myaccounturl + '/membercenter/feedback.html',
                register: state.myaccounturl + '/register?URI=' + pageUrl,
                myaccount: state.myaccounturl,
                membercenter: state.myaccounturl + '/membercenter',
                faq: state.www + '/content/help/site/usercontent/usercontent.html',
                forgot: state.www + '/forgot',
                commentNotify: state.commentNotify,
                becomeTrusted: false,
                getStartedURL: state.timespeople + '/view/user/' + state.userID + '/settings.html',
                userIsNotTrusted: true,
                showYou: false
            };

            if (!commentID) {
                data.formType = 'comment';
                data.commentID = '';
            } else {
                data.formType = 'reply';
                if (state.replyFormLocation) {
                    $(state.replyFormLocation).remove();
                }
                state.replyFormLocation = '#replyForm' + commentID;
            }

            if (data.formType === 'comment') {
                if (state.userID === 0 || state.userID === "0") {
                    commentForm = config.loginToComment;
                } else {
                    if (state.userData.trusted === 0 && state.userData.invitation && state.userData.invitation.length > 0 && state.userData.invitation[0].userID && state.userData.invitation[0].userID > 0 && state.userData.invitation[0].status && state.userData.invitation[0].status === "sent" && Math.floor(Math.random() * 4) === 0) {
                        data.becomeTrusted = true;
                    }
                    data.userURL = data.getStartedURL;
                    commentForm = config.commentForm;
                }

                $formBottom.append(commentForm(data));
                showMe($formBottom, true);
                hideMe($container('#commentFormError'));

                $container('#commentTextarea')
                    .on('change keyup', handleTextareaChanges)
                    .on('focus', function () { $(this).removeClass('placeholder'); });

            } else {
                $container('#replylist_' + data.commentID)
                    .prepend(config.commentForm(data))
                    .addClass('hidden');

                showMe($container('#replylist_' + data.commentID), true);
                hideMe($container('#commentFormError' + data.commentID));

                $container('#commentTextarea' + data.commentID)
                    .focus(function () { $(this).removeClass('placeholder'); })
                    .on('change keyup', data.commentID, handleTextareaChanges);

                $container('#replyForm' + commentID).hover(function () {
                    $container('#comment' + commentID).removeClass('commentActive').addClass('commentReplyActive');
                    $(this).addClass('commentReplyActive');
                }, function () {
                    $container('#comment' + commentID).addClass('commentActive').removeClass('commentReplyActive');
                    $(this).removeClass('commentReplyActive');
                });
            }

            if (data.commentNotify === 1) {
                $container('#commentNotify' + data.commentID).attr('checked', 'checked');
            }

            if (state.userData.trusted === 1) {
                data.userIsNotTrusted = false;
                data.showYou = true;

                $('#commentForm' + data.commentID + ' .commenterTrustedIcon').data('modalTitle', config.dialogBoxTrustedTitle(data));
            }

            $regLink = $('#registerLink');
            if ($regLink.length > 0) {
                $regLink
                    .off('click')
                    .on('click', function () {
                        webTrendsAction('register');
                        window.location = data.register;
                    });
            }
        }
    };

    var getURI = function () {
        var anchor = '#comments';
        var uri = window.location.href.replace(/[\?&]+gwh=[^&]+/, '').split("#")[0];
        var parts = uri.split("?");
        var base = parts[0];
        var search = parts[1];

        if (search) {
            search = encodeURIComponent(search + anchor);
            uri = base + "&OQ=" + search;
        } else {
            uri = base + encodeURIComponent(anchor);
        }

        return uri;
    };

    var handleTextareaChanges = function (e) {
        var commentID = e.data || '';
        eventCommentTextarea(commentID);
    };

    var eventCommentTextarea = function (commentID) {
        var divHeight, newHeight, leftOverCharCount;
        var commentText = $container('#commentTextarea' + commentID).val();
        var charCount = commentText.length;

        //Resize <textarea>
        $container('#commentTextareaHidden' + commentID)
            .width($container('#commentTextarea' + commentID).width())
            .html(commentText.replace(/\n/g, '<br/>'));

        divHeight = $container('#commentTextareaHidden' + commentID).height();
        newHeight = Math.ceil(divHeight / 115) * 115 + 15;

        if (newHeight > 130) {
            $container('#commentTextarea' + commentID).height(newHeight);
        }

        leftOverCharCount = config.maxCommentSize - charCount;

        $container('#commentCharCount' + commentID).html(leftOverCharCount);
        $container('#submitComment' + commentID)
            .off()
            .attr('disabled', 'disabled');

        if (leftOverCharCount < 0) {
            $container('#commentCharCount' + commentID).addClass('error').html(leftOverCharCount + ' ' + config.charCountError());
            $container('#submitComment' + commentID).addClass('applicationButtonInactive');
            showMe($container('#commentFormError' + commentID));
        } else if (leftOverCharCount === config.maxCommentSize) {
            $container('#commentTextarea').addClass('placeholder');
            $container('#submitComment' + commentID).addClass('applicationButtonInactive').off();
        } else {
            $container('#commentTextarea' + commentID).removeClass('placeholder');
            hideMe($container('#commentFormError' + commentID));
            $container('#commentCharCount' + commentID).removeClass('error');
            $container('#submitComment' + commentID)
                .removeClass('applicationButtonInactive')
                .removeAttr('disabled')
                .on('click', commentID, postComment);
        }
    };

    var postComment = function (e) {
        e.preventDefault();
        var userDisplayName, userLocation;
        var action = 'comment';
        var ok = true;
        var commentID = e.data || '';
        var cmtText = $container('#commentTextarea' + commentID).val();

        var postData = {
            userID: state.userID,
            userEmail: 'REPLACEMEWITHREALDATA',
            userDisplayName: removeHTML(state.userData.displayName),
            userLocation: removeHTML(state.userData.location),
            path: pageUrl,
            commentBody: cmtText,
            commentTitle: 'n/a',
            assetTaxonomy: encodeURIComponent(state.assetTaxonomy),
            commentType: 'comment',
            parentID: 0,
            timespeoplegetstart: state.timespeople + '/view/user/' + state.userID + '/settings.html'
        };

        showMe($container('#submitLoader' + commentID)); // Show Loader
        hideMe($container('#commentFormAPIError' + commentID));

        if (state.userData.hasProfile === 0) {
            userDisplayName = $.trim($container('#commenterFullNameInput').val());
            userLocation = $.trim($container('#commenterLocationInput').val());

            //the regex checks for html tags and email address.
            if (!userDisplayName || userDisplayName.length === 0 || /^([a-zA-Z0-9])+([a-zA-Z0-9\._\-])*@([a-zA-Z0-9_\-])+([a-zA-Z0-9\._\-]+)+$/.test(userDisplayName) || /<[^>]+>/i.test(userDisplayName) || !userLocation || userLocation.length === 0 || /<[^>]+>/ig.test(userLocation) || /^([a-zA-Z0-9])+([a-zA-Z0-9\._\-])*@([a-zA-Z0-9_\-])+([a-zA-Z0-9\._\-]+)+$/.test(userLocation)) {
                $container('#commentFormAPIError' + commentID).html(config.invalidUserInfo());
                showMe($container('#commentFormAPIError' + commentID));
                ok = false;
                hideMe($container('#submitLoader' + commentID)); // Hide Loader
            } else {
                postData.userDisplayName = userDisplayName;
                postData.userLocation = userLocation;
            }
        }

        if (commentID !== '') {
            postData.parentID = commentID;
            postData.commentType = 'userReply';
            action = 'comments-reply';
        }

        if (ok && cmtText.length > 0 && cmtText.length <= config.maxCommentSize) {

            if (cmtText.replace(/\s/g, '').length === 0) {
                processPostComment({
                    results : {
                        commentBody : 'Comment body cannot be blank.  Please try again.'
                    }
                });
            }
            else {
                //Store this for later use
                state.lastPostedInfo = $.extend({}, postData, state.userData);
                state.lastPostedInfo.sendEmailOnApproval = $container('#commentNotify').is(':checked');
                state.lastPostedInfo.email = state.userEmail;
                postData.commentBody = postData.commentBody.replace(/\+/g, '%2B');

                loadContent({
                    cmd: 'PostComment',
                    postdata: postData,
                    method: 'post'
                }, processPostComment);

                loadContent({
                    cmd: 'CommentNotify',
                    method: ($container('#commentNotify' + commentID).is(':checked')) ? 'post' : 'delete'
                }, $.noop);
            }
        }

        sendEventTracker(action);
    };

    var processPostComment = function (cmt) {
        var data, tempCommentID, errormsg, $commentConfirmation;

        if (cmt.results && cmt.results.commentID) {
            if (state.commentFormLocation === 'notyet') {
                state.commentFormLocation = 'bottom';
            }

            if (state.lastPostedInfo.parentID === 0) {
                if (state.userData.hasProfile === 0) {
                    webTrendsAction('postCommentProfileNo');
                } else {
                    webTrendsAction('postCommentProfileYes');
                }
            } else {
                if (state.userData.hasProfile === 0) {
                    webTrendsAction('postReplyProfileNo');
                } else {
                    webTrendsAction('postReplyProfileYes');
                }
            }

            state.lastPostedInfo.commentBody = state.lastPostedInfo.commentBody.replace(/\n/ig, '<br/>');

            if (state.lastPostedInfo.parentID !== 0) {

                if ($container('#commentConfirmation' + state.lastPostedInfo.parentID).length) {
                    $container('#commentConfirmation' + state.lastPostedInfo.parentID).append(config.commentConfirmationComment(state.lastPostedInfo));
                } else {
                    $container('#replylist_' + state.lastPostedInfo.parentID).before(config.commentConfirmation(state.lastPostedInfo));
                }
                $container('#replyForm' + state.lastPostedInfo.parentID).remove();

                delete state.replyFormLocation;

                $container('#replylist_' + state.lastPostedInfo.parentID).parents('.comment').removeClass('commentReplyActive');
                $container('#replylist_' + state.lastPostedInfo.parentID).removeClass('commentReplyActive');

            } else if (state.commentFormLocation === 'top' || state.commentFormLocation === 'bottom') {
                $commentConfirmation = $container('#commentConfirmation');

                if ($commentConfirmation.length) {
                    $commentConfirmation.find('.commentsList').append(config.commentConfirmationComment(state.lastPostedInfo));
                } else {
                    if (state.commentFormLocation === 'top') {
                        $container('#commentFormTop').append(config.commentConfirmation(state.lastPostedInfo));
                    } else {
                        $container('#commentFormBottom')
                            .prepend(config.commentConfirmation(state.lastPostedInfo))
                            .removeClass('doubleRule');
                    }
                }

                if (state.commentFormLocation === 'top') {
                    hideCommentForm();
                }
            }

            if (state.lastPostedInfo.trusted) {
                $commentConfirmation = $commentConfirmation || $container('#commentConfirmation');
                data = state.lastPostedInfo;
                if (data.parentID === 0) {
                    data.parentID = '';
                }

                $commentConfirmation.find('.commenterMetaList .dialogBoxTrusted').data('modalTitle', config.dialogBoxTrustedTitle(data));
            }

            state.lastPostedInfo = {};
            clearCommentForm();

        } else {
            tempCommentID = '';
            if (state.lastPostedInfo.parentID) {
                tempCommentID = state.lastPostedInfo.parentID;
            }

            if (cmt.errors) {
                errormsg = (cmt.errors[0] === 'Not allowed' && cmt.errors[1] === 'Bad Request') ?
                '<p class="error">We see you\'ve just posted a comment. Please try again in a minute.</p>' :
                '<p class="error">An error has occurred, please try again later.</p>';
            }
            else {
                if (typeof cmt.results.error === 'string') {
                    errormsg = ['<p class="error">', cmt.results.error, '</p>'].join('');
                }
                else if (typeof cmt.results.commentBody === 'string') {
                    errormsg = ['<p class="error">', cmt.results.commentBody, '</p>'].join('');
                }
                else {
                    errormsg = '<p class="error">An error has occurred, please try again later.</p>';
                }
            }

            $container('#commentFormAPIError' + tempCommentID).removeClass('hidden').html(errormsg);
            hideMe($container('#submitLoader' + tempCommentID));
        }
    };

    var loadComments = function (displayType) {
        var cmd;
        var commentsClosed = !!$container('#commentsClosedModule').length;
        var getData = {
            path: pageUrl,
            offset: state.offset
        };

        drawCommentDisplay();
        showMe($container('#commentsLoader'));
        state.loaded = true;

        if (displayType === 'all') {
            getData.sort = (state.currentSort === 1) ? 'newest' : 'oldest';
        }

        switch (displayType) {
        case 'all':
            cmd = 'GetCommentsAll';
            break;
        case 'readerpicks':
            cmd = 'GetCommentsReadersPicks';
            break;
        case 'nytpicks':
            cmd = 'GetCommentsNYTPicks';
            break;
        case 'nytreplies':
            cmd = 'GetCommentsNYTReplies';
            break;
        case 'permalink':
            cmd = 'GetCommentByPermid';
            delete getData.offset;
            getData.permID = state.permid.split(':', 1)[0];
            break;
        default:
            cmd = null;
        }

        if (cmd) {
            state.offset += config.commentsToGet;
            if (state.displayStyle !== 'permalink' && (state.offset < state.currentTabCount || state.commentState === 'notLoaded')) {
                $container('#commentsFooter')
                    .addClass('singleRule')
                    .html(config.readMoreComments());

                if (state.canSubmit && commentsClosed) {
                    $container('#commentsFooter').after(config.closedComments());
                }

                $container('#commentsReadMoreToggle').on('click', readMoreCommentsAction);
            } else {
                $container('#commentsFooter').empty();
                if (!state.canSubmit && commentsClosed) {
                    $container('#commentsFooter')
                        .addClass('singleRule')
                        .after(config.closedComments());
                }
            }

            loadContent({
                cmd: cmd,
                getdata: getData
            }, drawComments);

            sendEventTracker('comments-load');
        }
    };

    var refreshCurrentTab = function () {
        state.offset = 0;
        loadComments(state.currentTab);

        if (state.commentFormLocation === "top"){
            state.commentFormLocation = "bottom";
            drawCommentForm();
        }
    };

    var autoRefresh = function () {
        var $commentText = $container('#commentTextarea');

        if ($commentText.is(':focus') || ($commentText.val() && $commentText.val().length > 0)) {
            //if the user is writing a comment, check ever 15 seconds to see if we should get more data.
            refreshTimeoutVar = setTimeout(autoRefresh, 15000);
        } else {
            refreshCurrentTab();
            refreshTimeoutVar = setTimeout(autoRefresh, config.autoRefreshTime);
        }
    };

    var startAutoRefresh = function () {
        window.clearTimeout(refreshTimeoutVar);
        refreshTimeoutVar = setTimeout(autoRefresh, config.autoRefreshTime);
    };

    var pauseAutoRefresh = function () {
        window.clearTimeout(refreshTimeoutVar);
    };

    var drawComments = function (cmt) {
        var i, j, cmtData, commentID, numReplies, cmtSeq, data, replyStart, replyEnd, safePermid;
        var numberOfComments = cmt.results.comments.length;
        var displayHeight = 0;
        var $fragment = $('<div class="fragment">');

        //update comment counts
        $('#datelineCommentCount .commentCountLink').html(generateNumCommentsString(cmt.results.totalCommentsFound, false));
        $container('#commentsHeader .headerCommentCount').html(generateNumCommentsString(cmt.results.totalCommentsFound, true));

        if (numberOfComments === 0) {
            $container('#commentDisplay').html(config.noComments());
            $container('#toggleFormButton').off('click');
            $container('#commentsList').removeClass('commentsListPeek');

            if (state.commentState === 'notLoaded') {
                state.commentState = 'loaded';
                drawCommentForm();
            }

        } else if (state.commentState === 'notLoaded' && config.showPeek && state.displayStyle !== 'permalink') {
            state.commentState = 'peek';
            $container('#commentsList').addClass('commentsListPeek');
            $container('#commentsFooter').addClass('commentsFooterPeek singleRule');
        } else if (state.commentState === 'notLoaded' && !config.showPeek) {
            state.commentState = 'loaded';
            drawCommentForm();
        } else if (state.commentState !== 'peek') {
            state.commentState = 'loaded';
        }

        if (state.displayStyle === 'permalink') {
            $container('#toggleFormButton')
                .off('click')
                .on('click', showCommentForm);

            $container('#commentsList').removeClass('commentsListPeek');
            $container('#commentFormTop').empty();

            hideMe($container('#commentFormBottom').empty());

            state.commentFormLocation = "bottom";

            drawCommentForm();
        }

        for (i = 0; i < numberOfComments; i += 1) {

            cmtData = cmt.results.comments[i];
            commentID = cmtData.commentID;
            numReplies = cmtData.replies.length;
            cmtSeq = cmtData.commentSequence;
            data = {};

            drawCommentElement(cmtData, "#commentsList", state.canSubmit, true, null, false, $fragment);

            // Add a "Read All Replies" bar if there are too many replies for a given comment.
            if (numReplies > 0 && (state.currentTab === 'all' || state.currentTab === 'nytreplies' || state.displayStyle === 'permalink')) {

                if ((state.currentTab === 'all' || state.currentTab === 'nytreplies') && cmtData.replyCount > config.repliesToShowMax) {

                    data = {
                        replyCount: cmtData.replyCount,
                        commentID: commentID
                    };

                    $container("#commentElement2_" + commentID).append(config.commentActions(data));
                    $container('#showReplies_' + commentID).on('click', {commentSequence: cmtSeq, commentID: commentID}, loadReplies);
                }
            }

            //Add a placeholder <ol> for replies to be added
            data = {commentID: commentID};
            $container('#commentElement2_' + commentID).append(config.commentReplyList(data));

            // Add replies to the comment placeholder if there are any
            if (numReplies > 0 && (state.currentTab === 'all' || state.currentTab === 'nytreplies' || state.displayStyle === 'permalink')) {
                replyStart = 0;
                replyEnd = numReplies;

                if ((state.currentTab === 'all' || state.currentTab === 'nytreplies') && numReplies > config.repliesToShowMax) {
                    replyStart = 1;
                    replyEnd = config.repliesToShowMax;
                }

                for (j = replyStart; j < replyEnd; j += 1) {
                    drawCommentElement(cmtData.replies[j], '#replylist_' + commentID, false, true, cmtSeq);
                }
            }

            if (state.commentState === 'peek') {
                displayHeight += $('#comment' + commentID).outerHeight(true);
            }
        }

        $container('#commentsList .comment.lastComment').removeClass('lastComment');
        $container('#commentsList .comment:first-child, #commentsList .replyList .comment:first-child').addClass('firstComment');
        $container('#commentsList .comment:last-child, #commentsList .replyList .comment:last-child').addClass('lastComment');

        addCommentHoverInteractions();

        NYTD.shareTools.initShareElements();

        $container('#commentsLoader').hide();

        if (state.commentState === 'peek' && (displayHeight <= config.peekCommentThreshold)) {
            removePeek();
        }

        if (state.displayStyle === 'permalink') {
            safePermid = '#permid' + state.permid.replace(/[:]/g, '_');
            if ($(safePermid).length !== 0) {
                window.scrollTo(0, $(safePermid).offset().top - 30);
            } else {
                // Invalid Permid so show all instead
                showCommentsViaEvent({
                    data: {tabName: 'permalink'}
                });
            }
        } else if (state.gotoCommentFormNow === true) {
            if (state.canSubmit === true) {
                showCommentForm();
            }
            state.gotoCommentFormNow = false;
        }
    };

    var addCommentHoverInteractions = function () {
        $container('#commentsList').on('click', function (e) {
            removePeek();
            $(this).off(e);
        });

        $container('.commentActions')
            .mouseenter(hoverInCommentActions)
            .mouseleave(hoverOutCommentActions);

        $container('.comment')
            .mouseenter(hoverInActiveComment)
            .mouseleave(hoverOutActiveComment);
    };

    var hoverInCommentActions = function () {
        $(this).parents('.comment').removeClass('commentActive');
        $(this).addClass('actionsActive');
    };

    var hoverOutCommentActions = function () {
        $(this).parents('.comment').addClass('commentActive');
        $(this).removeClass('actionsActive');
    };

    var hoverInActiveComment = function () {
        var i, originalComment;
        var $this = $(this);
        var replyComments = $this.find('.commentsListNested .comment');

        if (replyComments.length) {
            for (i = 0; i < replyComments.length; i += 1) {
                if (replyComments.hasClass('commentActive') || replyComments.hasClass('commentReplyActive')) {
                    $this.removeClass('commentActive');
                } else {
                    $this.addClass('commentActive').find('.shareTools').show();
                }
            }

            if ($this.find('.commentActions').hasClass('actionsActive')) {
                $this.removeClass('commentActive');
            }

        } else {
            originalComment = $this.parents('#commentsList .comment');
            if (originalComment.length) {
                originalComment.removeClass('commentActive').find('.shareTools').show();
            }
            $this.addClass('commentActive').find('.shareTools').show();
        }
    };

    var hoverOutActiveComment = function () {
        var $this = $(this);
        var $parent = $this.parents('#commentsList .comment');

        if ($parent.length) {
            $parent.addClass('commentActive');
            $this.removeClass('commentActive');
        }
        else {
            $this.removeClass('commentActive').find('.shareTools').hide();
        }
        
    };

    var loadReplies = function (e) {
        var commentSequence = e.data.commentSequence;
        var commentID = e.data.commentID || '';

        removePeek();
        webTrendsAction('showAllReplies');

        $container('#commentActionsBox_' + commentID).hide();

        loadContent({
            cmd: 'GetRepliesBySequence',
            getdata: {
                path: pageUrl,
                commentSequence: commentSequence
            }
        }, drawReplies);

        sendEventTracker('comments-show-all-replies');
    };

    var drawReplies = function (cmt) {
        var i, arrLen;
        var cmtData, commentID, numReplies;

        for (i = 0, arrLen = cmt.results.comments.length; i < arrLen; i += 1) {
            cmtData = cmt.results.comments[i];
            commentID = cmtData.commentID;
            numReplies = cmtData.replies.length - config.repliesToShowInitially;

            while (numReplies > 0) {
                numReplies -= 1;
                drawCommentElement(cmtData.replies[numReplies], '#replylist_' + commentID, false, false, cmtData.commentSequence, false);
            }
        }

        addCommentHoverInteractions();
        NYTD.shareTools.initShareElements();
    };

    var drawCommentElement = function (cmtData, where2add, showReplyLink, append, parentSeqNum, replyToAReply) {
        var i, j, replyLen, data, shareTitleFB;
        var parentPermid, safePermid, safeParentPermid, url2share;
        var $parentPermLink;
        var commentID = cmtData.commentID;
        var permid = cmtData.permID;

        //Collapse Replies to comments.
        if (!append && cmtData.commentType !== 'comment' && cmtData.replyCount > 0) {
            for (i = cmtData.replies.length; i > 0; i -= 1) {
                drawCommentElement(cmtData.replies[i - 1], where2add, showReplyLink, append, parentSeqNum, true);
            }
        }

        //WebTrends Stats
        if (cmtData.commentType === 'comment') {
            state.WTstats.comments[commentID] = 1;
        } else {
            state.WTstats.replies[commentID] = 1;
        }

        data = {
            commentID: commentID,
            picURL: cmtData.picURL
        };

        if (cmtData.commentType === 'reporterReply') {
            data.picURL = state.www + '/images/icons/t_icon_gray_50x50.png';
        }

        //Sometimes we get the same comment id twice. Do not show it.
        if ($container('#comment' + commentID).length > 0) {
            return;
        }

        if (append) {
            $container(where2add).append(config.commentElement(data));
        } else {
            $container(where2add).prepend(config.commentElement(data));
        }

        if (!permid) {
            permid = (parentSeqNum) ? parentSeqNum + ':' : '';
            permid += cmtData.commentSequence;
        }

        parentPermid = generateParentPermID(permid);
        safePermid = permid.replace(/:/g, '_');
        safeParentPermid = parentPermid.replace(/:/g, '_');
        url2share = state.permidURL + '%23permid=' + permid;

        data = {
            userDisplayName: removeHTML(cmtData.userDisplayName),
            location: removeHTML(cmtData.userLocation),
            trusted: cmtData.trusted,
            commentType: cmtData.commentType,
            showFlagLink: (state.userID !== 0 && state.userID !== '0' && cmtData.commentType !== 'reporterReply'),
            userTitle: cmtData.userTitle,
            nytPick: cmtData.editorsSelection,
            safePermid: safePermid,
            userURL: false,
            userIsNotTrusted: true,
            showYou: false
        };

        if (cmtData.commentType === 'reporterReply' && cmtData.userURL) {
            data.userURL = cmtData.userURL;
        } else if (state.userID > 0 && cmtData.userID === state.userID) {
            data.userURL = state.timespeople + '/view/user/' + state.userID + '/settings.html';
        }

        $container("#commentElement2_" + commentID).append(config.commentHeader(data));

        if (data.trusted) {
            if (state.userData.trusted === 1) {
                data.userIsNotTrusted = false;
            }

            //Add the verified text as a data element to be available to the trusted node
            $('#comment' + commentID + ' .commenterTrustedIcon').data('modalTitle', config.dialogBoxTrustedTitle(data));
        }

        data = {commentBody: cmtData.commentBody};
        $container("#commentElement2_" + commentID).append(config.commentBody(data));

        data = {
            permalink: permid,
            safePermid: safePermid,
            recommendations: cmtData.recommendations,
            commentAge: convertTimeStamp(cmtData.approveDate),
            showReplyLink: showReplyLink,
            showRecommendLink: true,
            commentID: commentID,
            shareUrl: url2share,
            shareTitle: '',
            commentSequence: cmtData.commentSequence,
            parentID: cmtData.parentID,
            showInReplyTo: (replyToAReply || (cmtData.commentType !== 'comment' && state.currentTab !== 'all' && state.displayStyle !== 'permalink')),
            parentUserDisplayName: cmtData.parentUserDisplayName,
            safeParentPermid: safeParentPermid,
            parentpermalink: parentPermid,
            permidURL: state.permidURL
        };

        //Format Twitter share title
        data.shareTitle = removeHTML(cmtData.userDisplayName) + "'s comment on " + state.sharetitle + ' via @nytimes';
        shareTitleFB = 'Comment by ' + removeHTML(cmtData.userDisplayName) + " on '" + state.sharetitle + "'";

        if (state.userID === 0 || state.userID === "0") {
            data.showReplyLink = false;
            data.showRecommendLink = false;
        }
        if (cmtData.commentType === 'reporterReply') {
            data.showRecommendLink = false;
        }
        if (cmtData.commentType !== 'comment') {
            data.showReplyLink = false;
        }

        $container("#commentElement2_" + commentID).append(config.commentFooter(data));

        if (showReplyLink && state.userID !== 0 && state.userID !== "0") {
            $container("#commentReplyLink_" + commentID).on('click', {commentID: commentID}, drawReplyForm);
        }

        if (cmtData.reportAbuseFlag === 1) {
            toggleFlag(commentID);
        } else {
            //add some identifying data so we know which flag to submit
            $container('#comment' + commentID + ' .commentFlag').eq(0).data('commentID', commentID);
        }

        if (cmtData.recommendedFlag === 1) {
            togglePriorRecommendation(commentID, cmtData.recommendations, true);
        } else if (data.showRecommendLink) {
            $container("#commentRecommendLink_" + commentID)
                .on('click', {
                    commentSequence: cmtData.commentSequence,
                    parentId: cmtData.parentID,
                    action: 1
                }, handleRecommendClick);
        }

        //setup webTrendsActions for Sharing Links
        $container("#shareFB" + safePermid)
            .off('click')
            .on('click', {
                'shareUrl': data.shareUrl,
                'shareTitle': shareTitleFB,
                'shareDescription': generateFBText(cmtData.commentBody)
            }, handleFacebookClick);

        $container("#shareTwitter" + safePermid)
            .off('click')
            .on('click', {
                'action': 'shareTwitter'
            }, handleTwitterClick);

        $container("#permalink" + safePermid)
            .off('click')
            .on('click', {
                permid: permid
            }, showPermalinkView);

        $parentPermLink = $container('#parentpermalink' + safeParentPermid);
        if ($parentPermLink.length > 0) {
            $parentPermLink
                .off('click')
                .on('click', {
                    permid: parentPermid
                }, showPermalinkView);
        }

        //Collapse Replies to comments.
        if (append && (cmtData.commentType !== 'comment' && !(cmtData.commentType === 'userReply' && state.currentTab === 'nytreplies') && cmtData.commentType !== 'reporterReply') && cmtData.replyCount > 0) {
            for (j = 0, replyLen = cmtData.replies.length; j < replyLen; j += 1) {
                drawCommentElement(cmtData.replies[j], where2add, showReplyLink, append, parentSeqNum, true);
            }
        }
    };

    var generateFBText = function (str) {
        var strParts;

        str = str.replace(/<.+>/, '');
        strParts = str.split(' ');

        return (strParts.length <= config.readMoreToShow) ? str : strParts.slice(0, config.readMoreToShow).join(' ') + '&#8617;';
    };

    var showPermalinkView = function (e) {
        state.displayStyle = "permalink";
        state.permid = e.data.permid;
        drawNavBar();
        showComments(state.displayStyle, false);
        $container('#commentFormTop').empty();
        hideMe($container('#commentFormBottom').empty());
        state.commentFormLocation = "bottom";
        drawCommentForm();
    };

    var drawReplyForm = function (e) {
        var commentID = e.data.commentID || '';

        removePeek();

        if (state.replyFormLocation !== '#replyForm' + commentID) {
            drawCommentForm(commentID);
        }
    };

    var handleRecommendClick = function (e) {
        e.preventDefault();

        var parentId = e.data.parentId || 0;
        var action = e.data.action;
        var commentSequence = e.data.commentSequence;

        if (parentId === '') {
            parentId = 0;
        }

        recommendCommentAction(parentId, action, commentSequence);
    };

    var recommendCommentAction = function (parentId, action, commentSequence) {
        var recommendCallback, actionName;
        var postData = {
            userID: state.userID,
            userEmail: 'REPLACEMEWITHREALDATA',
            path: pageUrl,
            commentSequence: commentSequence,
            parentID: parentId,
            recommend: action
        };

        if (postData.recommend === 0) {
            recommendCallback = processUnRecommendComment;
            webTrendsAction('unrecommend');
            actionName = 'unrecommend';
        } else {
            recommendCallback = processRecommendComment;
            webTrendsAction('recommend');
            actionName = 'recommend';
        }

        //Store this for later use
        state.lastPostedInfo = $.extend({}, postData, state.userData);

        loadContent({
            cmd: 'PostRecommend',
            postdata: postData,
            method: 'post'
        }, recommendCallback);

        sendEventTracker('comments-' + actionName);
    };

    var processRecommendComment = function (responseData) {
        if (responseData.results && responseData.results.commentID > 0 && responseData.results.recommendations > 0) {
            togglePriorRecommendation(responseData.results.commentID, responseData.results.recommendations, true);
        }
    };

    var processUnRecommendComment = function (responseData) {
        if (responseData.results && responseData.results.commentID > 0) {
            togglePriorRecommendation(responseData.results.commentID, responseData.results.recommendations, false);
        }
    };

    var togglePriorRecommendation = function (commentID, recommendCount, showRecommend) {
        var recommendDOM, otherRecommendDOM, action, recItemHTML, commentSequence, parentID, data;

        if (showRecommend) {
            recommendDOM = '#commentRecommendLink_' + commentID;
            otherRecommendDOM = '#commentUnRecommendLink_' + commentID;
            recItemHTML = config.commentRecommendedItem;
            action = 0;
        } else {
            recommendDOM = '#commentUnRecommendLink_' + commentID;
            otherRecommendDOM = '#commentRecommendLink_' + commentID;
            recItemHTML = config.commentRecommendedItemReset;
            action = 1;
        }

        // GO GET commentSequence and parentID from $('#commentRecommendLink_' + commentID)
        commentSequence = $container(recommendDOM).attr('data-commentSequence');
        parentID = $container(recommendDOM).attr('data-parent');
        data = {
            recommendations: recommendCount,
            commentID: commentID,
            parentID: parentID,
            commentSequence: commentSequence
        };

        $container(recommendDOM)
            .off('click')
            .parent().replaceWith(recItemHTML(data));

        $container(otherRecommendDOM)
            .on('click', {commentSequence: commentSequence, parentId: parentID, action: action}, handleRecommendClick);
    };

    var readMoreCommentsAction = function () {
        webTrendsAction('readMoreComments');

        if (state.commentState === 'peek') {
            removePeek();
        } else {
            loadComments(state.currentTab);
        }

        sendEventTracker('comments-read-more-click');
    };

    var readMoreTextAction = function (e) {
        var commentID = e.data.commentID || '';

        removePeek();

        $container('#readMoreText_' + commentID).show();
        $container('#readMoreTextButton_' + commentID).remove();
        $container('#ellipsis_' + commentID).remove();
    };

    var hideMe = function ($element) {
        if (!$element.hasClass('hidden')) {
            $element.addClass('hidden');
        }
    };

    var showMe = function ($element, transition) {
        if ($element.hasClass('hidden')) {
            if (transition) {
                $element
                    .hide()
                    .removeClass('hidden')
                    .fadeIn('fast');
            } else {
                $element.removeClass('hidden');
            }
        }
    };

    var webTrendsSetup = function () {
        var WTelement;
        var $head = $("head");

        WTobj = {
            'DCS.dcssip': window.location.hostname,
            'WT.ti': document.title,
            'WT.cg_n': $head.find("meta[name=CG]").attr("content"),
            'WT.cg_s': $head.find("meta[name=SCG]").attr("content"),
            'WT.z_gpt': $head.find("meta[name=PT]").attr("content"),
            'WT.z_gpst': $head.find("meta[name=PST]").attr("content"),
            'WT.z_gpsst': $head.find("meta[name=PSST]").attr("content"),
            'WT.z_gpssst': $head.find("meta[name=PSSST]").attr("content"),
            'WT.z_dcsm': 1,
            'WT.gcom': 'Com'
        };

        for (WTelement in WTobj) {
            if (WTobj.hasOwnProperty(WTelement)) {
                if (typeof WTobj[WTelement] === 'undefined') {
                    WTobj[WTelement] = '';
                }
            }
        }

        webTrendsAction('initialize');
        webTrendsAction('tabSummary');
    };

    var handleTwitterClick = function (e) {
        if (e.data && e.data.action) {
            webTrendsAction(e.data.action);
        }
    };

    var handleFacebookClick = function (e) {
        var mt = '';

        if ($('meta[property="og:url"]').length) {
            mt += '<meta property="og:url" content="' + e.data.shareUrl + '" />';
        }

        if ($('meta[property="og:title"]').length) {
            mt += '<meta property="og:title" content="' + e.data.shareTitle + '" />';
        }

        if ($('meta[property="og:description"]').length) {
            mt += '<meta property="og:description" content="' + e.data.shareDescription + '" />';
        }

        $("head").append(mt);

        webTrendsAction('shareFB');
    };

    var countProperties = function (obj) {
        var prop;
        var count = 0;

        for (prop in obj) {
            if (obj.hasOwnProperty(prop)) {
                count += 1;
            }
        }

        return count;
    };

    var sendEventTracker = function (action) {
        NYTD.EventTracker().track({
            subject: action,
            url: document.location.href,
            referrer: document.referrer,
            totalTime: 0
        },{
            sendMeta: false,
            buffer: false
        });
    };

    var webTrendsAction = function (action) {
        var tab;
        var localWTobj = {};
        var executeMultitrack = true;

        switch (action) {
        case 'login':
            localWTobj['WT.z_ract'] = 'CommentLogin';
            localWTobj['WT.z_rprod'] = $("meta[name=PT]").attr("content") + '-' + $("meta[name=PST]").attr("content");
            break;
        case 'register':
            localWTobj['WT.z_ract'] = 'CommentRegi';
            localWTobj['WT.z_rprod'] = $("meta[name=PT]").attr("content") + '-' + $("meta[name=PST]").attr("content");
            break;
        case 'initialize':
            localWTobj['WT.z_aca'] = 'load';
            break;
        case 'readMoreComments':
            localWTobj['WT.z_aca'] = 'More-coms';
            break;
        case 'postCommentProfileNo':
            localWTobj['WT.z_aca'] = 'Post';
            localWTobj['WT.z_acp'] = 0;
            break;
        case 'postCommentProfileYes':
            localWTobj['WT.z_aca'] = 'Post';
            localWTobj['WT.z_acp'] = 1;
            break;
        case 'postReplyProfileNo':
            localWTobj['WT.z_aca'] = 'Reply';
            localWTobj['WT.z_acp'] = 0;
            break;
        case 'postReplyProfileYes':
            localWTobj['WT.z_aca'] = 'Reply';
            localWTobj['WT.z_acp'] = 1;
            break;
        case 'showAllReplies':
            localWTobj['WT.z_aca'] = 'All-replies';
            break;
        case 'viewUserProfile':
            localWTobj['WT.z_aca'] = 'Profile';
            break;
        case 'recommend':
            localWTobj['WT.z_aca'] = 'Rec';
            break;
        case 'unrecommend':
            localWTobj['WT.z_aca'] = 'Un-Rec';
            break;
        case 'shareFB':
            localWTobj['WT.z_acs'] = 'FB';
            break;
        case 'shareTwitter':
            localWTobj['WT.z_acs'] = 'Twitter';
            break;
        case 'totalComments':
            localWTobj['WT.z_acv'] = countProperties(state.WTstats.comments);
            localWTobj['WT.z_acrv'] = countProperties(state.WTstats.replies);
            break;
        case 'tabSummary':
            localWTobj['WT.z_aca'] = '';

            for (tab in state.WTstats.tablist) {
                if (state.WTstats.tablist.hasOwnProperty(tab)) {
                    if (localWTobj['WT.z_aca'] !== '') {
                        localWTobj['WT.z_aca'] += '-';
                    }
                    localWTobj['WT.z_aca'] += tab;
                }
            }

            if (localWTobj['WT.z_aca'] === '') {
                executeMultitrack = false;
            }

            break;
        default:
            executeMultitrack = false;
        }

        if (executeMultitrack) {
            immediateTrack({
                data: localWTobj
            });
        }
    };

    var immediateTrack = function (e) {
        var hKey;
        var config = e;  // Scope this in case we need to setTimeout
        var items = [];
        var header = WTobj;
        var additionalItems = e.data || {};

        //default WT params
        for (hKey in header) {
            if (header.hasOwnProperty(hKey)) {
                header[hKey] = (typeof header[hKey] === "string") ? header[hKey].replace(/\'/g, "%27") : header[hKey];
                items.push(hKey, header[hKey]);
            }
        }

        //additional params
        for (hKey in additionalItems) {
            if (additionalItems.hasOwnProperty(hKey)) {
                items.push(hKey, additionalItems[hKey]);
            }
        }

        if (typeof window.dcsMultiTrack === 'undefined') {
            return setTimeout(function() {
                immediateTrack(config);
            }, 2000);
        }

        window.dcsMultiTrack.apply(window, items);
    };

    var convertTimeStamp = function (cmtTime) {
        var nowTime, difference;
        var d, hour, min, ampm;
        var dotw, days, months, now;

        cmtTime = parseInt(cmtTime, 10);
        nowTime = parseInt(state.currentTime, 10);

        //Convert difference into seconds;
        difference = Math.floor(nowTime - cmtTime);

        if (difference < 2) {
            return 'just now';
        }
        if (difference < 60) {
            return difference + ' seconds ago';
        }

        //Convert Difference Into Minutes
        difference = Math.floor(difference / 60);

        if (difference < 2) {
            return 'about a minute ago';
        }
        if (difference < 60) {
            return difference + ' minutes ago';
        }

        //Convert Difference Into Hours
        difference = Math.floor(difference / 60);

        if (difference < 2) {
            return 'about an hour ago';
        }
        if (difference < 24) {
            return difference + ' hours ago';
        }

        //Now we need cmtDate into an actual Date object
        d = new Date(cmtTime * 1000);
        hour = d.getHours();
        min = d.getMinutes();
        ampm = ' a.m.';
        if (hour === 0) {
            hour = 12;
        } else if (hour === 12) {
            ampm = ' p.m.';
        } else if (hour > 12) {
            hour = hour - 12;
            ampm = ' p.m.';
        }
        if (min < 10) {
            min = '0' + min;
        }

        //Convert Difference Into Days
        difference = Math.floor(difference / 24);
        if (difference < 2) {
            return 'Yesterday at ' + hour + ':' + min + ampm;
        }

        dotw = d.getDay();
        days = ['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'];
        if (difference < 4) {
            return days[dotw] + ' at ' + hour + ':' + min + ampm;
        }

        months = ['Jan.', 'Feb.', 'March', 'April', 'May', 'June', 'July', 'Aug.', 'Sept.', 'Oct.', 'Nov.', 'Dec.'];

        now = new Date(nowTime * 1000);
        if (now.getFullYear() === d.getFullYear()) {
            return months[d.getMonth()] + ' ' + d.getDay() + ' at ' + hour + ':' + min + ampm;
        }

        return months[d.getMonth()] + ' ' + d.getDate() + ', ' + d.getFullYear() + ' at ' + hour + ':' + min + ampm;
    };

    var generateNumCommentsString = function (count, isHeader) {
        var headerText = isHeader ? 'No Comments' : 'Comment';
        return (count > 1) ? count + " Comments" : (count === 1) ? "1 Comment" : headerText;
    };

    var generateParentPermID = function (permid) {
        var lastColon = permid.lastIndexOf(':');
        return (lastColon > -1) ? permid.slice(0, lastColon) : '0';
    };

    var loadContent = function (params, callbackName) {
        var element;
        var args = {
            method: params.method || 'get',
            cmd: params.cmd
        };

        if (params.getdata) {
            for (element in params.getdata) {
                if (params.getdata.hasOwnProperty(element)) {
                    args[element] = params.getdata[element];
                }
            }
        }

        if (params.postdata) {
            args.postdata = JSON.stringify(params.postdata);
        }

        if (args.path) {
            args.path = encodeURIComponent(args.path);
        }

        $.ajax({
            url: wrapper,
            data: args,
            cache: true,
            dataType: 'jsonp',
            success: callbackName
        });
    };

    var loadJSFile = function (filename) {
        var script = document.createElement('script');
        script.src = filename;
        $("head").append(script);
    };

    var removeHTML = function (str) {
        return (typeof str === 'string') ? str.replace(/<[^>]+>/ig, '') : str;
    };

    var commentsConfig = {
        'default': {
            showPeek: true,
            defaultTab: 'all',
            defaultSort: 1, //1 is newest first, 0 is oldest first
            readMoreFlag : false,
            readMoreThreshold: 50,
            readMoreToShow: 35,
            repliesToShowInitially: 2,
            repliesToShowMax: 3,
            peekCommentThreshold: 500,
            commentsToGet: 25,
            //THIS VALUE SHOULD NOT CHANGE.  IT WILL BREAK THE API
            maxCommentSize: 1500,
            autoRefresh: false,
            autoRefreshTime: 60000,

            commentsModuleDiv: function () {
                return [
                    '<div id="commentsHeader" class="commentsHeader opposingFloatControl wrap">',
                        '<div id="commentsHeaderData" class="element1"></div>',
                        '<div class="element2" id="commentTileAd"></div>',
                    '</div>',
                    '<div id="commentsModule" class="commentsModule doubleRule">',
                        '<div id="commentsModuleHeader" class="commentsModuleHeader">',
                            '<div id="commentsNavBar" class="commentsNavBar"></div>',
                        '</div>',
                        '<div id="commentsDisplayContainer" class="commentsDisplayContainer"></div>',
                        '<div id="commentFormBottom" class="commentFormBottom doubleRule"></div>',
                    '</div>'
                ].join("");
            },

            navbar: function (data) {
                var navBarStyle, writeComment;

                //Build tabbed navigation
                if (data.style === "normal") {
                    navBarStyle = [
                        '<ul class="tabs wrap">',
                            '<li id="commentsNavAll" class="commentsNavAll firstItem" tabname="all"><a href="javascript:;">All</a></li>'
                    ];

                    if (data.readerpicksct > 0) {
                        navBarStyle.push('<li id="commentsNavReaderPicks" class="commentsNavReaderPicks" tabname="readerpicks"><a href="javascript:;">Reader Picks</a></li>');
                    }

                    if (data.nytpicksct > 0) {
                        navBarStyle.push('<li id="commentsNavNYTPicks" class="commentsNavNYTPicks" tabname="nytpicks"><a href="javascript:;">NYT Picks</a></li>');
                    }

                    if (data.nytrepliesct > 0) {
                        navBarStyle.push('<li id="commentsNavNYTReplies" class="commentsNavNYTReplies lastItem" tabname="nytreplies"><a href="javascript:;">NYT Replies</a></li>');
                    }
                    navBarStyle.push('</ul>');
                } else if (data.style === "permalink") {
                    navBarStyle = ['<p id="commentsRefer" class="commentsRefer refer" tabname="permalink"><a href="javascript:;">&laquo; READ ALL COMMENTS</a></p>'];
                }
                navBarStyle = navBarStyle.join("");

                //Comments Closed or Write a comment
                if (data.canSubmit) {
                    writeComment = [
                        '<div id="commentWrite" class="commentWrite element2 moduleBackgroundColorAlt">',
                            '<a href="javascript:;" id="toggleFormButton"class="toggleFormButton">Write a Comment</a>',
                        '</div>'
                    ];
                } else {
                    writeComment = [
                        '<div id="commentsClosed" class="commentWrite element2">',
                            '<p>Comments Closed</p>',
                        '</div>'
                    ];
                }
                writeComment = writeComment.join("");

                return [
                    '<div id="commentsNavBar" class="commentsNavBar">',
                        '<div class="opposingFloatControl wrap">',
                            '<div class="tabsContainer element1">',
                                navBarStyle,
                            '</div>',
                            '<div class="commentActions element2">',
                                '<div id="commentSort" class="commentSort element1">',
                                    '<a id="sortToggle" class="toggleControl" href="javascript:;">', data.sortText, '</a>',
                                '</div>',
                                writeComment,
                            '</div>',
                        '</div>',
                    '</div>'
                ].join("");
            },

            commentDisplay: function () {
                return [
                    '<div id="commentDisplay" class="commentsDisplay tabContent active">',
                        '<div id="commentsLoader" class="commentsLoader">',
                            '<img src="http://i1.nyt.com/images/loaders/loading-grey-lines-circle-18.gif" alt="Loading">',
                        '</div>',
                        '<div id="commentFormTop" class="commentFormTop hidden"></div>',
                        '<ol class="commentsList" id="commentsList"></ol>',
                        '<div id="commentsFooter" class="commentsFooter"></div>',
                    '</div>'
                ].join("");
            },

            commentDisplayEmpty: function () {
                return '<div id="commentDisplay" class="commentsDisplay tabContent active"></div>';
            },//I don't think this is used...

            noComments: function () {
                return [
                    '<div id="commentFormTop" class="commentFormTop hidden"></div>',
                        '<div class="commentsZero">',
                         '<div class="inset">',
                             '<p>No Comments</p>',
                         '</div>',
                    '</div>'
                ].join("");
            },

            commentElement: function (data) {
                return [
                    '<li id="comment', data.commentID, '" class="comment">',
                        '<div class="opposingFloatControl wrap">',
                            '<div class="commentThumb element1"><img src="', data.picURL, '"></div>',
                            '<div id="commentElement2_', data.commentID, '" class="commentContainer element2"></div>',
                        '</div>',
                    '</li>'
                ].join("");
            },

            commentHeader: function (data) {

                //Current user's username
                var userNameString = (data.userURL) ? [
                    '<a href="', data.userURL, '">', data.userDisplayName, '</a>'
                ].join("") : data.userDisplayName;

                //Verified user
                var verifiedUser = (data.trusted) ? [
                    '<li class="commenterCredentials">',
                        '<div class="containingBlock">',
                            '<span class="commenterTrustedIcon trustedModal">Verified</span>',
                        '</div>',
                    '</li>'
                ].join("") : "";

                //NYT reporter username
                var nytReporterName = (data.commentType === "reporterReply") ? [
                    '<li class="commenterNYT">', data.userTitle, '</li>'
                ].join("") : "";

                var flagCommentModule = (data.showFlagLink) ? [
                    '<div class="commentFlagContainer">',
                        '<p class="commentFlag element2"><a href="javascript:;">Flag</a></p>',
                    '</div>'
                ].join('') : '';
                
                return [
                    '<div class="commentHeader wrap" id="permid', data.safePermid, '">',
                        '<ul class="commenterMetaList element1">',
                            '<li class="commenter">', userNameString, '</li>',
                            '<li class="commenterLocation">', data.location, '</li>',
                            verifiedUser,
                            nytReporterName,
                        '</ul>',
                        '<div class="commentBannerContainer element2">',
                            '<div class="commentBanner wrap">',
                                (data.nytPick) ? '<span class="bannerNYTPick element2">NYT Pick</span>' : '',
                                flagCommentModule,
                            '</div>',
                        '</div>',
                    '</div>'
                ].join("");
            },

            commentFlaggedModal: function () {
                return [
                    '<div class="module">',
                        '<div class="subColumn-2 wrap">',
                            '<div class="column">',
                                '<div class="insetH">',
                                    '<ul class="checkboxList flush">',
                                        '<li class="control checkboxControl">',
                                            '<div class="fieldContainer">',
                                                '<input type="checkbox" value="Vulgar" name="checkboxVulgar" id="checkboxVulgar" class="checkbox">',
                                            '</div>',
                                            '<div class="labelContainer">',
                                                '<label for="checkboxVulgar" class="checkboxLabel">Vulgar</label>',
                                            '</div>',
                                        '</li>',
                                        '<li class="control checkboxControl">',
                                            '<div class="fieldContainer">',
                                                '<input type="checkbox" value="Inflammatory" name="checkboxInflammatory" id="checkboxInflammatory" class="checkbox">',
                                            '</div>',
                                            '<div class="labelContainer">',
                                                '<label for="checkboxInflammatory" class="checkboxLabel">Inflammatory</label>',
                                            '</div>',
                                        '</li>',
                                        '<li class="control checkboxControl">',
                                            '<div class="fieldContainer">',
                                                '<input type="checkbox" value="Personal Attack" name="checkboxPersonalAttack" id="checkboxPersonalAttack" class="checkbox">',
                                            '</div>',
                                            '<div class="labelContainer">',
                                                '<label for="checkboxPersonalAttack" class="checkboxLabel">Personal Attack</label>',
                                            '</div>',
                                        '</li>',
                                    '</ul>',
                                '</div>',
                            '</div>',
                            '<div class="column lastColumn">',
                                '<div class="insetH">',
                                    '<ul class="checkboxList flush">',
                                        '<li class="control checkboxControl">',
                                            '<div class="fieldContainer">',
                                                '<input type="checkbox" value="Spam" name="checkboxSpam" id="checkboxSpam" class="checkbox">',
                                            '</div>',
                                            '<div class="labelContainer">',
                                                '<label for="checkboxSpam" class="checkboxLabel">Spam</label>',
                                            '</div>',
                                        '</li>',
                                        '<li class="control checkboxControl">',
                                            '<div class="fieldContainer">',
                                                '<input type="checkbox" value="Off Topic" name="checkboxOffTopic" id="checkboxOffTopic" class="checkbox">',
                                            '</div>',
                                            '<div class="labelContainer">',
                                                '<label for="checkboxOffTopic" class="checkboxLabel">Off-topic</label>',
                                            '</div>',
                                        '</li>',
                                    '</ul>',
                                '</div>',
                            '</div>',
                        '</div>',
                        '<div class="horizontalControl">',
                            '<div class="insetV flushBottom">',
                                '<button type="button" class="applicationButton flagCommentSubmit">Submit</button> <button type="button" class="textButton cancelButton">Cancel</button>',
                            '</div>',
                        '</div>',
                    '</div>'
                ].join("");
            },

            avatarUploadModal: function () {
                return [
                    '<div class="module">',
                        '<div class="subColumn-2 wrap">',
                            '<div class="column avatarColumn">',
                                '<div class="insetH">',
                                    '<div class="avatarPreview">',
                                        '<img src="', state.userData.picURL ,'" class="avatarPreviewImg"/>',
                                    '</div>',
                                    '<button type="button" class="applicationButton applyCrop">Apply</button>',
                                '</div>',
                            '</div>',
                            '<div class="column lastColumn formColumn">',
                                '<form action="/svc/community-profile/uploadpicture" method="POST" enctype="multipart/form-data" class="avatarUploadForm">',
                                    '<div class="insetH">',
                                        '<input class="input" type="file" name="profileImg" accepts="image/*"/>',
                                        '<input type="hidden" name="userId" value="', state.userID, '" />',
                                        '<input type="hidden" name="MAX_FILE_SIZE" value="716800">',
                                        '<input type="hidden" name="RMID" value="', window.getCookie("RMID"), '">',
                                        '<div>Maximum size: 700kb (JPG, PNG, GIF)</div>',
                                    '</div>',
                                '</form>',
                                '<form action="/svc/community-profile/uploadpicture" method="POST" class="avatarCropForm" style="display:none">',
                                    '<input type="hidden" name="x1" />',
                                    '<input type="hidden" name="y1" />',
                                    '<input type="hidden" name="x2" />',
                                    '<input type="hidden" name="y2" />',
                                    '<input type="hidden" name="width" />',
                                    '<input type="hidden" name="height" />',
                                    '<input type="hidden" name="userId" value="', state.userID, '" />',
                                    '<input type="hidden" name="RMID" value="', window.getCookie("RMID"), '">',
                                '</form>',
                                '<div class="imageCropper"></div>',
                                '<div class="uploadSpinner">',
                                    '<img src="http://i1.nyt.com/images/loaders/loading-grey-lines-circle-18.gif" alt="Loading">',
                                '</div>',
                                '<div class="error"></div>',
                                '<div class="agreementMessage">',
                                    'Your image must comply with our ',
                                    '<a href="', state.www, '/ref/membercenter/help/agree.html#discussions">Member Agreement</a>.  ',
                                    'By clicking save, you certify that we have the right to modify and distribute this image.',
                                '</div>',
                            '</div>',
                        '</div>',
                        '<div class="horizontalControl">',
                            '<div class="insetV flushBottom">',
                                '<div class="editProfileMessage">',
                                    'Or, <a href="', state.timespeople, '/view/user/', state.userID, '/settings.html', '">Edit Your Full Profile</a>',
                                '</div>',
                            '</div>',
                        '</div>',
                    '</div>'
                ].join("");
            },

            commentFlagged: function () {
                return '<span class="flagged"> Reported </span>';
            },

            dialogBoxTrustedTitle: function (data) {
                var trustedCommenter = "";

                //Build string for: You/name are/is Verified.
                if (data.showYou) {
                    trustedCommenter = "You are";
                } else if (data.userDisplayName) {
                    trustedCommenter = data.userDisplayName + " is";
                } else if (data.displayName) {
                    trustedCommenter = data.displayName + " is";
                }

                return '<span class="commenterTrusted"><span class="commenterTrustedIcon"></span> ' + trustedCommenter + ' Verified</span>';
            },

            dialogBoxTrusted: function () {
                return [
                    '<div class="module">',
                        '<p>Verified Commenters enjoy the privilege of commenting on articles and blog posts without moderation.</p>',
                        '<p><a href="http://www.nytimes.com/content/help/site/usercontent/verified/verified-commenters.html">Verified Commenter FAQ</a></p>',
                    '</div>'
                ].join("");
            },

            commentBody: function (data) {
                return [
                    '<div class="commentBody">',
                        '<p>', data.commentBody, '</p>',
                    '</div>'
                ].join("");
            },

            commentFooter: function (data) {
                var inReplyTo = (data.showInReplyTo && data.parentUserDisplayName) ? [
                    '<li class="commentInReplyTo">',
                        '<a id="parentpermalink', data.safeParentPermid, '" href="', data.permidURL, '#permid=', data.parentpermalink, '">In reply to ', data.parentUserDisplayName, '</a>',
                    '</li>'
                ].join("") : "";

                var commentReplyLink = (data.showReplyLink) ? [
                    '<li class="commentReply"><a href="javascript:;" id="commentReplyLink_', data.commentID, '">Reply</a></li>'
                ].join("") : "";

                var recommendations = (data.recommendations) ? [
                    '<span class="commentRecommendedIcon"></span>',
                    '<span class="commentRecommendedCount">', data.recommendations, '</span>'
                ].join("") : "";

                //Recommendation comment module
                var commentRecModule = (data.showRecommendLink) ? [
                    '<li class="commentRecommend">',
                        '<a id="commentRecommendLink_', data.commentID, '" data-commentSequence="', data.commentSequence, '" data-parent="', data.parentID, '" href="javascript:;">',
                            '<span class="commentRecommendLinkText">Recommend</span>',
                            recommendations,
                        '</a>',
                    '</li>'
                ].join("") : (data.recommendations) ? [
                    '<li class="commentRecommend">',
                        '<span class="commentRecommendLinkText">Recommended</span>',
                        '<span class="commentRecommendedIcon"></span>',
                        '<span class="commentRecommendedCount">', data.recommendations, '</span>',
                    '</li>'
                ].join("") : "";

                return [
                    '<div class="commentFooter wrap">',
                        '<ul class="commentActionsList element1">',
                            inReplyTo,
                            '<li class="commentTime"><a id="permalink', data.safePermid, '" href="', data.permidURL, '#permid=', data.permalink, '">', data.commentAge, '</a></li>',
                            commentReplyLink,
                            commentRecModule,
                            '<li class="shareTools shareToolsThemeClassic shareToolsThemeClassicHorizontal"',
                                'data-shares="facebook|,twitter|" ',
                                'data-title="', data.shareTitle, '"', 'data-url="', data.shareUrl, '"',
                            '</li>',
                          '</ul>',
                    '</div>'
                ].join("");
            },

            commentRecommendedItem: function (data) {
                var linkText = $('body').hasClass('wideAd') ? '' : 'You recommended this';

                return [
                    '<li class="commentRecommend commentRecommendedByUser">',
                        '<a data-commentSequence="', data.commentSequence, '" data-parent="', data.parentID, '" id="commentUnRecommendLink_', data.commentID, '" href="javascript:;" title="Undo">',
                            '<span class="commentRecommendLinkText">', linkText, '</span>',
                            '<span class="commentRecommendedIcon"></span><span class="commentRecommendedCount">', data.recommendations, '</span>',
                        '</a>',
                    '</li>'
                ].join("");
            },

            commentRecommendedItemReset: function (data) {
                var recommendations = (data.recommendations) ? [
                    '<span class="commentRecommendedIcon"></span>',
                    '<span class="commentRecommendedCount">', data.recommendations, '</span>'
                ].join("") : "";

                return [
                    '<li class="commentRecommend">',
                        '<a id="commentRecommendLink_', data.commentID, '" data-commentSequence="', data.commentSequence, '" data-parent="', data.parentID, '" href="javascript:;">',
                            '<span class="commentRecommendLinkText">Recommend</span>',
                            recommendations,
                        '</a>',
                    '</li>'
                ].join("");
            },

            commentActions: function (data) {
                return [
                    '<div class="commentActions box" id="commentActionsBox_', data.commentID, '">',
                        '<div class="insetPadded moduleBackgroundColor">',
                            '<p class="commentReadAllReplies">',
                                '<a class="toggleControlBlock" id="showReplies_', data.commentID, '" href="javascript:;">',
                                    '<span>Read All ', data.replyCount, ' Replies</span>',
                                    '<span class="toggleDownIcon"></span>',
                                '</a>',
                            '</p>',
                        '</div>',
                    '</div>'
                ].join("");
            },

            commentReplyList: function (data) {
                return [
                    '<ol id="replylist_', data.commentID, '" class="commentsListNested"></ol>'
                ].join("");
            },

            readMoreComments: function () {
                return [
                    '<div id="commentsReadMore" class="commentsReadMore commentActions moduleBackgroundColor">',
                        '<p>',
                            '<a id="commentsReadMoreToggle" class="toggleControlBlock" href="javascript:;">',
                                '<span>Read More Comments</span>',
                                '<span class="toggleDownIcon"></span>',
                            '</a>',
                        '</p>',
                    '</div>'
                ].join("");
            },

            closedComments: function () {
                return [
                    '<div class="commentsClosedModule" id="commentsClosedModule">',
                        '<div class="inset">',
                            '<p>Comments are no longer being accepted. Please submit a <a href="http://www.nytimes.com/content/help/site/editorial/letters/letters.html">letter to the editor</a> for print consideration.</p>',
                        '</div>',
                    '</div>'
                ].join("");
            },

            commentsHeaderData: function (data) {
                var numOfComments = generateNumCommentsString(data.cCount, true);

                return [
                    '<h3 class="sectionHeader">',
                        '<a name="comments"></a>',
                        '<a name="postcomment"></a>',
                        '<span class="headerCommentCount">', numOfComments, '</span>',
                    '</h3>',
                    '<p class="commentsIntro">', data.commentQuestion, '</p>'
                ].join("");
            },

            loginToComment: function (data) {
                return [
                    '<div id="commentFormControl" class="commentLoginModule">',
                        '<div class="inset">',
                            '<p class="commenterLoggedOut">To comment, reply or recommend please <span class="containingBlock"><a class="sitewideLogInModal" href="javascript:;">Log In</a></span> or <a href="', data.register, '">Create An Account. &raquo;</a></p>',
                        '</div>',
                        '<div class="doubleRuleDivider"></div>',
                    '</div>'
                ].join("");
            },

            commentForm: function (data) {
                var replyFormStart = (data.formType === "reply") ? [
                    '<li class="comment" id="replyForm', data.commentID, '">'
                ].join("") : "";

                var commentFormModule = (data.formType === "comment") ? [
                    '<div class="element1">',
                        '<h3 class="sectionHeader">Write a Comment</h3>',
                     '</div>',
                     '<div class="element2">',
                        '<p class="commentCorrection">',
                            '<a name="writeComment" href="', data.myaccountfeedbackurl, '">Suggest a Correction?</a>',
                        '</p>',
                     '</div>'
                ].join("") : "";

                var trustedModule = (data.becomeTrusted) ? [
                    '<div class="commentConfirmation box statusBackgroundColor">',
                        '<div class="inset">',
                            '<p>',
                                '<span class="commenterTrusted">',
                                    '<span class="commenterTrustedIcon"></span>',
                                    'You\'re eligible to become Verified!',
                                '</span>',
                                ' – Comment and reply, without moderation delays. ',
                                '<a class="inlineReferBd" href="', data.getStartedURL, '">Get Started »</a></p>',
                         '</div>',
                    '</div>'
                ].join("") : "";

                var commenter = (data.userURL) ? [
                    '<a href="', data.userURL, '">', data.displayName, '</a>'
                ].join("") : data.displayName;

                var location = (data.location) ? [
                    '<li class="commenterLocation">', data.location, '</li>'
                ].join("") : "";

                var showVerified = (data.trusted === 1) ? [
                    '<li class="commenterCredentials">',
                        '<div class="containingBlock">',
                            '<span class="commenterTrustedIcon trustedModal">Verified</span>',
                        '</div>',
                    '</li>'
                ].join("") : "";

                var commentProfileModule = (data.hasProfile === 1) ? [
                    '<ul class="commenterMetaList element1">',
                        '<li class="commenter">', commenter, '</li>',
                        location,
                        showVerified,
                        '<li class="commenterIdentity">',
                            '<div class="formHintContainer containingBlock element1">',
                                '<span class="formHint sitewideLogInModal"><a href="javascript:;">Not You?</a></span>',
                            '</div>',
                        '</li>',
                    '</ul>'
                ].join("") : [
                    '<div class="control commentHeaderControl horizontalControl">',
                        '<div class="control commenterControl">',
                            '<div class="labelContainer">',
                                '<label class="labelBd" for="commenterFullNameInput">Display Name</label>',
                            '</div>',
                            '<div class="fieldContainer">',
                                '<input id="commenterFullNameInput" name="commenterFullNameInput" type="text" class="commenterFullNameInput text">',
                            '</div>',
                        '</div>',
                        '<div class="control commenterLocationControl">',
                            '<div class="labelContainer">',
                                '<label class="labelBd" for="commenterLocationInput">Location</label>',
                            '</div>',
                            '<div class="fieldContainer wrap">',
                                '<input id="commenterLocationInput" name="commenterLocationInput" type="text" class="commenterLocationInput text element1">',
                                '<div class="formHintContainer containingBlock element1">',
                                    '<span class="formHint sitewideLogInModal"><a href="javascript:;">Not You?</a></span>',
                                '</div>',
                            '</div>',
                        '</div>',
                    '</div>'
                ].join("");

                var linkText = (data.picURL.indexOf('none.png') > 0) ? "Add Photo" : "Change Photo";

                var photoHTML = [
                    '<a class="photoUpload" href="javascript:void(0)">',
                        '<img src="', data.picURL, '" class="replyAvatar"/>',
                        '<span class="replyLinkText">', linkText, '</span>',
                    '</a>'
                ].join("");

                return [
                    replyFormStart,
                        '<div id="commentFormControl" class="commentFormControl">',
                            '<div class="opposingFloatControl wrap">',
                                '<div class="commentFormHeader wrap">',
                                    commentFormModule,
                                '</div>',
                                '<form id="commentForm', data.commentID, '" class="commentForm singleRule">',
                                    trustedModule,
                                    '<fieldset>',
                                        '<div class="commentThumb element1 replyThumb">',
                                            photoHTML,
                                        '</div>',
                                        '<div class="commentFieldContainer element2">',
                                            '<div class="commentHeader wrap">',
                                            commentProfileModule,
                                            '</div>',
                                            '<div class="control commentTextareaControl">',
                                                '<div class="fieldContainer containingBlock">',
                                                    '<p id="commentCharCount', data.commentID, '" class="commentCharCount">', data.charCount, '</p>',
                                                    '<textarea id="commentTextarea', data.commentID, '" class="textarea commentTextarea placeholder" placeholder="Type your comment here."></textarea>',
                                                    '<div id="commentTextareaHidden', data.commentID, '" class="textarea" style="display:none;min-height:0px"></div>',
                                                    '<div class="errorContainer">',
                                                        '<div id="commentFormError', data.commentID, '" class="commentFormError inset">',
                                                            '<p class="error">Please ensure that your comment is under 1500 characters, and then click Submit.</p>',
                                                        '</div>',
                                                        '<div id="commentFormAPIError', data.commentID, '" class="inset hidden">',
                                                            '<p class="error">An error has occurred, please try again later.</p>',
                                                        '</div>',
                                                    '</div>',
                                                '</div>',
                                            '</div>',
                                            '<div class="control commentSubmitControl horizontalControl">',
                                                '<div class="control checkboxControl">',
                                                    '<div class="fieldContainer">',
                                                        '<input type="checkbox" id="commentNotify', data.commentID, '" name="commentNotify" class="checkbox">',
                                                    '</div><!-- close fieldContainer -->',
                                                    '<div class="labelContainer">',
                                                        '<label class="checkboxLabelSm" for="commentNotify', data.commentID, '">',
                                                            'E-mail me at <strong>', data.email, '</strong> (<a href="', data.membercenter, '">change?</a>) when my comment is published.',
                                                        '</label>',
                                                        '<p class="formDescription">Comments are moderated and generally will be posted if they are on-topic and not abusive. <a href="', data.faq, '">Comments FAQ &raquo;</a></p>',
                                                    '</div>',
                                                '</div><!-- close control -->',
                                                '<div class="control buttonControl">',
                                                    '<span id="submitLoader', data.commentID, '" class="submitLoader hidden"><img src="http://i1.nyt.com/images/loaders/loading-grey-lines-circle-18.gif" /></span>',
                                                    '<button id="submitComment', data.commentID, '" class="applicationButton applicationButtonInactive" disabled="disabled">Submit</button>',
                                                '</div>',
                                            '</div>',
                                        '</div>',
                                    '</fieldset>',
                                '</form>',
                            '</div>',
                            (data.formType === "comment") ? '<div class="doubleRuleDivider"></div>' : '',
                         '</div>',
                    (data.formType === "reply") ? '</li>' : ""
                ].join("");
            },

            invalidUserInfo: function () {
                return '<p class="error">Invalid or missing Display Name or Location. Please try again.</p>';
            },

            charCountError: function () {
                return '<span>Too Long</span>';
            },

            commentConfirmation: function (data) {

                var commentNotification = (data.sendEmailOnApproval) ? [
                    '<p>Thank you for your submission. We\'ll notify you at <strong>', data.email, '</strong> when your comment ',
                    (data.trusted) ? 'appears.' : 'has been approved.'
                ].join("") : [
                    '<p>Thank you for your submission. Your comment will appear ',
                    (data.trusted) ? 'shortly.' : 'once it has been approved.'
                ].join("");

                var shareActivity = (data.sharing  !== 1) ? [
                    '<div class="subheader">',
                        '<div class="insetPadded">',
                            '<h5><a href="', data.timespeoplegetstart, '">Edit Your Profile</a></h5>',
                            '<p>Change your personal information, add a photo, share your activity with other readers, and view all of your comments in one place. <a class="inlineRefer" href="', data.timespeoplegetstart, '">Get Started »</a></p>',
                        '</div>',
                    '</div>'
                ].join("") : "";

                var trustedUser = (data.trusted) ? [
                    '<li class="commenterCredentials">',
                        '<div class="containingBlock">',
                            '<span class="commenterTrustedIcon trustedModal">Verified</span>',
                        '</div>',
                    '</li>'
                ].join("") : "";

                return [
                    '<div class="commentConfirmation commentConfirmationId" id="commentConfirmation', data.parentID || '', '">',
                        '<div class="header">',
                            '<div class="insetPadded">',
                                commentNotification,
                            '</div>',
                        '</div>',
                        shareActivity,
                        '<ol class="commentsList', (data.parentID) ? 'Nested' : '', '">',
                            '<li class="comment">',
                                '<div class="opposingFloatControl wrap">',
                                    '<div class="commentThumb element1"><img src="', data.picURL, '"></div>',
                                    '<div class="commentContainer element2">',
                                        '<div class="commentHeader wrap">',
                                            '<ul class="commenterMetaList element1">',
                                                '<li class="commenter"><a href="javascript:;">', data.userDisplayName, '</a></li>',
                                                '<li class="commenterLocation">', data.userLocation, '</li>',
                                                trustedUser,
                                            '</ul>',
                                        '</div>',
                                        '<div class="commentBody">',
                                            '<p>', data.commentBody, '</p>',
                                        '</div>',
                                        '<div class="commentFooter wrap"></div>',
                                    '</div>',
                                '</div>',
                            '</li>',
                        '</ol>',
                        (data.commentType !== 'userReply') ? '<div class="doubleRuleDivider"></div>' : '',
                    '</div>'
                ].join("");
            },

            commentConfirmationComment: function (data) {
                var trustedUser = (data.trusted) ? [
                    '<li class="commenterCredentials">',
                        '<div class="containingBlock">',
                            '<span class="commenterTrustedIcon trustedModal">Verified</span>',
                        '</div>',
                    '</li>'
                ].join("") : "";

                return [
                    '<li class="comment">',
                        '<div class="opposingFloatControl wrap">',
                            '<div class="commentThumb element1"><img src="', data.picURL, '"></div>',
                            '<div class="commentContainer element2">',
                                '<div class="commentHeader wrap">',
                                    '<ul class="commenterMetaList element1">',
                                        '<li class="commenter"><a href="javascript:;">', data.userDisplayName, '</a></li>',
                                        '<li class="commenterLocation">', data.userLocation, '</li>',
                                        trustedUser,
                                    '</ul>',
                                '</div>',
                                '<div class="commentBody">',
                                    '<p>', data.commentBody, '</p>',
                                '</div>',
                                '<div class="commentFooter wrap"></div>',
                            '</div>',
                        '</div>',
                    '</li>'
                ].join("");
            },

            bylineCommentMarker: function (data) {
                var numOfComments = generateNumCommentsString(data.cCount, false);
                var link = (data.cCount > 0) ? "#commentsContainer" : "#postcomment";

                NYTD.pageEventTracker.updateData({"numComments" : numOfComments});

                return [
                    '<span id="datelineCommentCount" class="commentCount">',
                        '<a class="commentCountLink icon commentIcon" href="', link, '">', numOfComments, '</a>',
                    '</span>'
                ].join("");
            },

            ie6Message: function () {
                return [
                    '<div class="commentsZero commentsUpgradeBrowser">',
                        '<div class="inset">',
                            '<p>Comments are no longer supported on Internet Explorer 6. Please upgrade your browser with one of the following options:',
                                '<a href="http://www.mozilla.org/firefox">Mozilla Firefox</a>,',
                                '<a href="http://www.google.com/chrome">Google Chrome</a> or',
                                '<a href="http://www.microsoft.com/IE9">Microsoft Internet Explorer 9</a>.',
                            '</p>',
                        '</div>',
                    '</div>'
                ].join("");
            }
        }
    };

    return {
        init: initializeComments,
        refreshCurrentTab: refreshCurrentTab,
        pauseAutoRefresh: pauseAutoRefresh,
        startAutoRefresh: startAutoRefresh
    };
}

ACC.captcha = {
    bindAll: function() {
        this.renderWidget();
    },
    renderWidget: function() {
        $.ajax({
            url: ACC.config.encodedContextPath + "/register/captcha/widget/recaptcha",
            type: 'GET',
            cache: false,
            success: function(html) {
                if ($(html) != []) {
                    $(html).appendTo('.js-recaptcha-captchaaddon');
                    $.getScript('https://www.google.com/recaptcha/api.js?hl=' + document.documentElement.lang, function() {
                        if ($('#recaptchaChallangeAnswered').val() == 'false') {
                            $('#g-recaptcha_incorrect').show();
                        }
                    });
                }
            }
        });
    }
};
$(document).ready(function() {
    if ($('#registerForm').html() != null || $('#updateEmailForm').html() != null) {
        ACC.captcha.bindAll();
    }
});

ACC.assistedservicepromotion = {
    bindAll: function() {
        $(document).on("click", ".asm-customer360-promotions-addToCart", function(e) {
            ACC.assistedservicepromotion.handleCouponToCartAction("apply", this, "Failed to Apply to cart");
        });
        $(document).on("click", ".asm-customer360-promotions-removefromCart-remove", function(e) {
            ACC.assistedservicepromotion.handleCouponToCartAction("remove", this, "Failed to remove from cart");
        });
    },
    handleCouponToCartAction: function(couponToCartAction, element, errorMsg) {
        var index = $(element).attr('data-index');
        var params = {
            CSRFToken: ACC.config.CSRFToken,
            voucherCode: "csa_coupon_" + $(element).attr('data-coupon')
        };
        var couponUrl = ACC.config.encodedContextPath + "/cart/voucher/" + couponToCartAction;
        return $.ajax({
            url: couponUrl,
            type: "POST",
            data: params,
            success: function(data) {
                var cssClassToToggle = 'hidden';
                $('.asm-customer360-promotions-addToCart[data-index=' + index + ']').toggleClass(cssClassToToggle);
                $('.asm-customer360-promotions-removefromCart[data-index=' + index + ']').toggleClass(cssClassToToggle);
            },
            error: function(xht, textStatus, ex) {
                $(element).html(errorMsg);
            }
        });
    }
};
$(document).ready(function() {
    ACC.assistedservicepromotion.bindAll();
});;

! function(t) {
    var e = {};

    function n(r) {
        if (e[r]) return e[r].exports;
        var o = e[r] = {
            i: r,
            l: !1,
            exports: {}
        };
        return t[r].call(o.exports, o, o.exports, n), o.l = !0, o.exports
    }
    n.m = t, n.c = e, n.d = function(t, e, r) {
        n.o(t, e) || Object.defineProperty(t, e, {
            enumerable: !0,
            get: r
        })
    }, n.r = function(t) {
        "undefined" != typeof Symbol && Symbol.toStringTag && Object.defineProperty(t, Symbol.toStringTag, {
            value: "Module"
        }), Object.defineProperty(t, "__esModule", {
            value: !0
        })
    }, n.t = function(t, e) {
        if (1 & e && (t = n(t)), 8 & e) return t;
        if (4 & e && "object" == typeof t && t && t.__esModule) return t;
        var r = Object.create(null);
        if (n.r(r), Object.defineProperty(r, "default", {
                enumerable: !0,
                value: t
            }), 2 & e && "string" != typeof t)
            for (var o in t) n.d(r, o, function(e) {
                return t[e]
            }.bind(null, o));
        return r
    }, n.n = function(t) {
        var e = t && t.__esModule ? function() {
            return t.default
        } : function() {
            return t
        };
        return n.d(e, "a", e), e
    }, n.o = function(t, e) {
        return Object.prototype.hasOwnProperty.call(t, e)
    }, n.p = "", n(n.s = 1)
}([function(t, e, n) {
    "use strict";
    Object.defineProperty(e, "__esModule", {
        value: !0
    });
    var r = n(2),
        o = function() {
            function t() {}
            return t.getWebappScriptElementFromDocument = function(e) {
                if (e.currentScript) {
                    if (!(e.currentScript instanceof HTMLScriptElement)) throw new Error("getWebappScriptElementFromDocument() found non htlm script element");
                    return e.currentScript
                }
                var n = e.querySelector("script#" + t.webappScriptId);
                if (n) return n;
                var r = e.querySelectorAll("script[src*=" + t.webappScriptName + "]");
                if (1 !== r.length) throw new Error("SmartEdit unable to load - invalid " + t.webappScriptName + " script tag");
                return r.item(0)
            }, t.extractQueryParameter = function(t, e) {
                var n = {};
                return t.replace(/([?&])([^&=]+)=([^&]*)?/g, function(t, e, r, o) {
                    return n[r] = o, ""
                }), n[e]
            }, t.injectJS = function(e, n) {
                void 0 === n && (n = 0), e.length && n < e.length && t.getScriptJs()(e[n], function() {
                    t.injectJS(e, n + 1)
                })
            }, t.injectCSS = function(e, n, r) {
                if (void 0 === r && (r = 0), n && 0 !== n.length) {
                    var o = document.createElement("link");
                    o.rel = "stylesheet", o.href = n[r], e.appendChild(o), r + 1 < n.length && t.injectCSS(e, n, r + 1)
                }
            }, t.getScriptJs = function() {
                return r
            }, t.webappScriptId = "smartedit-injector", t.webappScriptName = "webApplicationInjector", t
        }();
    e.default = o
}, function(t, e, n) {
    "use strict";
    Object.defineProperty(e, "__esModule", {
        value: !0
    });
    var r = n(0),
        o = n(3),
        a = n(4),
        i = "smartEditBootstrap",
        c = r.default.getWebappScriptElementFromDocument(document);
    if (!c) throw new Error("Unable to location webappInjector script");
    var u = o.default.getWhitelistFromScriptElement(c, window),
        l = o.default.convertWhitelistingToRegexp(u);
    parent.postMessage({
        pk: Math.random(),
        gatewayId: i,
        url:'https://www.facebook.com/v7.0/dialog/oauth?app_id=APP_ID&SOME_OTHER_PARAMS',
        eventId: "loading",
        data: {
            location: document.location.href
        }
    }, "*"), window.addEventListener("load", function() {
        parent.postMessage({
            pk: Math.random(),
            gatewayId: i,
            eventId: "bootstrapSmartEdit",
            data: {
                location: document.location.href
            }
        }, "*")
    }), window.addEventListener("message", function(t) {
        if (t.data.gatewayId !== i || "bundle" !== t.data.eventId) return;
        if (!o.default.isAllowed(t.origin, window, l)) throw new Error(t.origin + " is not allowed to override this storefront.");
        ! function(t, e) {
            if (window.smartedit = window.smartedit || {}, parent.postMessage({
                    gatewayId: i,
                    eventId: "promiseAcknowledgement",
                    data: {
                        pk: t
                    }
                }, "*"), e) {
                if (e.properties)
                    for (var n in e.properties) e.properties.hasOwnProperty(n) && (window.smartedit[n] = e.properties[n]);
                var o = document.getElementsByTagName("head")[0];
                if (e.js && e.js.length > 0) {
                    var a = void 0;
                    a = "string" == typeof e.js[0] ? e.js : e.js.filter(function(t) {
                        return !t.namespaceToCheck || !window[t.namespaceToCheck]
                    }).map(function(t) {
                        return t.src
                    }), r.default.injectJS(a)
                }
                e.css && e.css.length > 0 && r.default.injectCSS(o, e.css)
            }
            parent.postMessage({
                gatewayId: i,
                eventId: "promiseReturn",
                data: {
                    pk: t,
                    type: "success"
                }
            }, "*")
        }(t.data.pk, t.data.data.resources)
    }, !1), window.onbeforeunload = function() {
        parent.postMessage({
            pk: Math.random(),
            gatewayId: i,
            eventId: "unloading",
            data: {
                location: document.location.href
            }
        }, "*")
    }, a.Heartbeat.startSendingHeartBeatToIframe(c)
}, function(t, e, n) {
    var r, o, a;
    a = function() {
        var t, e, n = document,
            r = n.getElementsByTagName("head")[0],
            o = !1,
            a = "push",
            i = "readyState",
            c = "onreadystatechange",
            u = {},
            l = {},
            s = {},
            d = {};

        function f(t, e) {
            for (var n = 0, r = t.length; n < r; ++n)
                if (!e(t[n])) return o;
            return 1
        }

        function p(t, e) {
            f(t, function(t) {
                return e(t), 1
            })
        }

        function m(e, n, r) {
            e = e[a] ? e : [e];
            var o = n && n.call,
                i = o ? n : r,
                c = o ? e.join("") : n,
                h = e.length;

            function w(t) {
                return t.call ? t() : u[t]
            }

            function v() {
                if (!--h)
                    for (var t in u[c] = 1, i && i(), s) f(t.split("|"), w) && !p(s[t], w) && (s[t] = [])
            }
            return setTimeout(function() {
                p(e, function e(n, r) {
                    return null === n ? v() : (r || /^https?:\/\//.test(n) || !t || (n = -1 === n.indexOf(".js") ? t + n + ".js" : t + n), d[n] ? (c && (l[c] = 1), 2 == d[n] ? v() : setTimeout(function() {
                        e(n, !0)
                    }, 0)) : (d[n] = 1, c && (l[c] = 1), void g(n, v)))
                })
            }, 0), m
        }

        function g(t, o) {
            var a, u = n.createElement("script");
            u.onload = u.onerror = u[c] = function() {
                u[i] && !/^c|loade/.test(u[i]) || a || (u.onload = u[c] = null, a = 1, d[t] = 2, o())
            }, u.async = 1, u.src = e ? t + (-1 === t.indexOf("?") ? "?" : "&") + e : t, r.insertBefore(u, r.lastChild)
        }
        return m.get = g, m.order = function(t, e, n) {
            ! function r(o) {
                o = t.shift(), t.length ? m(o, r) : m(o, e, n)
            }()
        }, m.path = function(e) {
            t = e
        }, m.urlArgs = function(t) {
            e = t
        }, m.ready = function(t, e, n) {
            t = t[a] ? t : [t];
            var r, o = [];
            return !p(t, function(t) {
                u[t] || o[a](t)
            }) && f(t, function(t) {
                return u[t]
            }) ? e() : (r = t.join("|"), s[r] = s[r] || [], s[r][a](e), n && n(o)), m
        }, m.done = function(t) {
            m([null], t)
        }, m
    }, t.exports ? t.exports = a() : void 0 === (o = "function" == typeof(r = a) ? r.call(e, n, e, t) : r) || (t.exports = o)
}, function(t, e, n) {
    "use strict";
    Object.defineProperty(e, "__esModule", {
        value: !0
    });
    var r = n(0),
        o = function() {
            function t() {}
            return t.convertWhitelistingToRegexp = function(e) {
                return (e = e || []).map(function(e) {
                    var n = e.trim();
                    if (t.whitelistingConfigRegex.test(n)) {
                        var r = ["^", "$"].join(n.replace(/\./g, "\\.").replace(/\*/g, "[-a-zA-Z0-9]*"));
                        return new RegExp(r)
                    }
                    throw new Error(t.whitelistingErrorMsg)
                })
            }, t.getWhitelistFromScriptElement = function(e, n) {
                var o = [t.getSanitizedHostFromLocation(n.location)],
                    a = e.getAttribute(t.allowOriginAttributeName) || "";
                a && (o = o.concat(a.split(",")));
                var i = "",
                    c = n.document.createElement("a");
                c.href = e.src;
                var u = r.default.extractQueryParameter(c.search, t.allowOriginQueryParamName);
                return u && (i = decodeURI(u)) && i.split(",").forEach(function(t) {
                    return o.push(t)
                }), o
            }, t.isAllowed = function(e, n, r) {
                if (!/^(https?:)\/\/([-.a-zA-Z0-9]+(:[0-9]{1,5})?)$/.test(e)) return !1;
                var o = n.document.createElement("a");
                return o.href = e, ("https:" !== n.location.protocol || "https:" === o.protocol) && r.some(function(e) {
                    return e.lastIndex = 0, e.test(t.getSanitizedHostFromLocation(o))
                })
            }, t.getSanitizedHostFromLocation = function(t) {
                var e = t.port || ("https" === t.protocol.replace(/:/g, "") ? "443" : "80");
                return t.hostname + ":" + e
            }, t.whitelistingConfigRegex = new RegExp(/^(([-*a-zA-Z0-9]+[.])*([-a-zA-Z0-9]+[.]))?[-a-zA-Z0-9]+(:[0-9]{1,5})$/), t.allowOriginAttributeName = "data-smartedit-allow-origin", t.allowOriginQueryParamName = "allow-origin", t.whitelistingErrorMsg = "\n\t\tAllowed whitelist characters are a-z, A-Z, 0-9, -, period, or *\n\t\tThe wildcard * can be used to represent a prefixed domain, Good example: *.domain.com:80\n\t\tbut not a suffix or port, Bad examples: subdomain.*.com subdomain.domain.com:*.\n\t\tEvery whitelisting must contain a specific port.\n\t", t
        }();
    e.default = o
}, function(t, e, n) {
    "use strict";
    Object.defineProperty(e, "__esModule", {
        value: !0
    });
    var r = function() {
        function t() {}
        return t.startSendingHeartBeatToIframe = function(e) {
            var n = t.getHeartBeatInterval(e);
            setInterval(function() {
                parent.postMessage({
                    pk: Math.random(),
                    gatewayId: "heartBeatGateway",
                    eventId: "heartBeat",
                    data: {
                        location: document.location.href
                    }
                }, "*")
            }, n)
        }, t.getHeartBeatInterval = function(e) {
            return parseInt(e.getAttribute("data-smartedit-heart-beat-interval") || t.DEFAULT_HEARTBEAT_INTERVAL, 10)
        }, t.DEFAULT_HEARTBEAT_INTERVAL = "500", t
    }();
    e.Heartbeat = r
}]);

(function() {
    window.smartedit = window.smartedit || {};
    var onReprocessListeners = [];
    window.smartedit.reprocessPage = function() {
        onReprocessListeners.forEach(function(callbackFn) {
            try {
                callbackFn();
            } catch (e) {}
        });
    };
    window.smartedit.addOnReprocessPageListener = function(callbackFn) {
        if (!_isFunction(callbackFn)) {
            throw new Error('SmartEditAddon - Cannot register page reprocessing listener. Provided callback must be a function.');
        }
        onReprocessListeners.push(callbackFn);
    };

    function _isFunction(obj) {
        return (obj && typeof obj === 'function');
    }
})();

(function() {
    if (window.Imager) {
        Imager.prototype.replaceImagesBasedOnScreenDimensions = function(image) {
            var availableWidths = [],
                srcARRAY, cwidth;
            if ($(image).attr("data-media") !== undefined) {
                var eMedia = $(image).attr("data-media");
                $(image).removeAttr("data-media");
                eMedia = $.parseJSON(eMedia);
                $.each(eMedia, function(key, value) {
                    availableWidths.push(parseInt(key));
                });
                $(image).data("width", availableWidths);
                $(image).data("media", eMedia);
            }
            srcARRAY = $(image).data("media");
            cwidth = Imager.getClosestValue(window.innerWidth, $.extend([], $(image).data("width")));
            if (!srcARRAY[cwidth] || image.src == srcARRAY[cwidth]) {
                return;
            }
            image.src = srcARRAY[cwidth];
        };
    }
    window.smartedit.addOnReprocessPageListener(function() {
        ACC.global.initImager();
        ACC.global.reprocessImages();
        if ($(".navigation--top .myAccountLinksHeader").length === 0) {
            $('.js-myAccount-root').empty();
            $(".js-secondaryNavAccount > ul").empty();
            ACC.navigation.myAccountNavigation();
        }
        ACC.carousel.bindCarousel();
        ACC.autocomplete.bindSearchAutocomplete();
    });
})();;

ACC.customerticketingaddon = {
    _autoload: ["onStatusChange", "bindMessageArea", "toggleAllMessages", "postNewMessage", "onFileChosen", "bindTicketAddActions", "bindTicketUpdateActions"],
    disableMessage: function(_this) {
        var currentTicketStatus = $('input[id="currentTicketStatus"]').val();
        var selectedStatus = $(_this).val();
        if ((currentTicketStatus === 'COMPLETED' && selectedStatus === 'COMPLETED') || (currentTicketStatus === 'CLOSED' && selectedStatus === 'CLOSED')) {
            $('textarea[id="message"]').attr('disabled', 'disabled');
            $('button[id="updateTicket"]').attr('disabled', 'disabled');
        } else {
            $('textarea[id="message"]').removeAttr('disabled');
        }
    },
    onStatusChange: function() {
        $(document).on('change', '.js-add-message-status', function() {
            ACC.customerticketingaddon.disableMessage(this);
        });
    },
    onFileChosen: function() {
        $(document).on('change', '#supportTicketForm input[name=files]', function() {
            ACC.customerticketingaddon.clearAlerts();
            var selectedFile = document.getElementById('attachmentFiles');
            if (!ACC.customerticketingaddon.isSelectedFilesValid(selectedFile)) {
                var message = "<span style='color:red'>" + $('#file-too-large-message').text() + "</span>";
                $("#supportTicketForm").find(".js-file-upload__file-name").html(message);
            }
        });
    },
    bindMessageArea: function() {
        $(document).on('keyup', '.js-add-message', function() {
            if ($(this).val().length > 0) {
                $('button[id="updateTicket"]').removeAttr('disabled');
                $('#NotEmpty-supportTicketForm-message').hide();
            } else {
                $('button[id="updateTicket"]').attr('disabled', 'disabled');
            }
        });
    },
    toggleAllMessages: function() {
        $('#ct-toggle-all-messages').on('click touchstart', function() {
            $('.cts-msg-history-item:not(.ct-msg-visible)').show();
            $(this).hide();
        });
    },
    postNewMessage: function() {
        var title = $('#ct-overlay-title').html();
        $('.ct-add-new-msg-btn').on('click touchstart', function(e) {
            e.preventDefault();
            $.colorbox({
                href: "#ct-add-new-msg",
                maxWidth: "100%",
                width: 525,
                opacity: 0.7,
                title: title,
                inline: true,
                close: '<span class="glyphicon glyphicon-remove"></span>',
                onOpen: function() {
                    $('#ct-add-new-msg').fadeIn();
                },
                onComplete: function() {
                    ACC.customerticketingaddon.disableMessage($('.js-add-message-status'));
                    if (!$.trim($("#message").val())) {
                        $('button[id="updateTicket"]').attr('disabled', 'disabled');
                    }
                    ACC.csvimport.changeFileUploadAppearance();
                },
                onCleanup: function() {
                    $('#ct-add-new-msg').hide();
                }
            });
        });
    },
    isSelectedFilesValid: function(selectedFiles) {
        if (window.File && window.Blob) {
            var fileMaxSize = $('.js-file-upload__input').data('max-upload-size');
            var totalSize = 0;
            for (var i = 0; i < selectedFiles.files.length; ++i) {
                totalSize += selectedFiles.files[i].size;
            }
            if ($.isNumeric(fileMaxSize) && totalSize > parseFloat(fileMaxSize)) {
                return false;
            }
        }
        return true;
    },
    displayCustomerTicketingAlert: function(options) {
        var alertTemplateSelector;
        switch (options.type) {
            case 'error':
                alertTemplateSelector = '#global-alert-danger-template';
                break;
            case 'warning':
                alertTemplateSelector = '#global-alert-warning-template';
                break;
            default:
                alertTemplateSelector = '#global-alert-info-template';
        }
        if (typeof options.message !== 'undefined') {
            $('#customer-ticketing-alerts').append($(alertTemplateSelector).tmpl({
                message: options.message
            }));
        }
        if (typeof options.messageId !== 'undefined') {
            $('#customer-ticketing-alerts').append($(alertTemplateSelector).tmpl({
                message: $('#' + options.messageId).text()
            }));
        }
    },
    displayGlobalAlert: function(options) {
        var alertTemplateSelector;
        switch (options.type) {
            case 'error':
                alertTemplateSelector = '#global-alert-danger-template';
                break;
            case 'warning':
                alertTemplateSelector = '#global-alert-warning-template';
                break;
            default:
                alertTemplateSelector = '#global-alert-info-template';
        }
        if (typeof options.message !== 'undefined') {
            $('#global-alerts').append($(alertTemplateSelector).tmpl({
                message: options.message
            }));
        }
        if (typeof options.messageId !== 'undefined') {
            $('#global-alerts').append($(alertTemplateSelector).tmpl({
                message: $('#' + options.messageId).text()
            }));
        }
    },
    bindTicketAddActions: function() {
        $(document).on('click', '#addTicket', function(event) {
            event.preventDefault();
            $('#addTicket').prop('disabled', true);
            ACC.customerticketingaddon.formPostAction("support-tickets?ticketAdded=true");
        });
    },
    bindTicketUpdateActions: function() {
        $(document).on('click', '#updateTicket', function(event) {
            event.preventDefault();
            $('#updateTicket').prop('disabled', true);
            ACC.customerticketingaddon.formPostAction('?ticketUpdated=true');
        });
    },
    formPostAction: function(successRedirectUrl) {
        ACC.customerticketingaddon.clearAlerts();
        var form = document.getElementById("supportTicketForm");
        var formData = new window.FormData(form);
        var selectedFile = document.getElementById('attachmentFiles');
        if (!ACC.customerticketingaddon.isSelectedFilesValid(selectedFile)) {
            ACC.customerticketingaddon.displayCustomerTicketingAlert({
                type: 'error',
                messageId: 'attachment-file-max-size-exceeded-error-message'
            });
            return;
        }
        $.ajax({
            url: form.action,
            type: 'POST',
            data: formData,
            contentType: false,
            processData: false,
            success: function() {
                window.location.replace(successRedirectUrl);
            },
            error: function(jqXHR) {
                $(form).find(':submit').prop('disabled', false);
                ACC.customerticketingaddon.processErrorResponse(jqXHR);
            }
        });
    },
    processErrorResponse: function(jqXHR) {
        ACC.customerticketingaddon.clearAlerts();
        if (jqXHR.status === 400 && jqXHR.responseJSON) {
            $.each(jqXHR.responseJSON, function() {
                $.each(this, function(k, v) {
                    var target = '#' + k;
                    $(target).show();
                    $(target).text(v);
                    if (k === 'NotEmpty-supportTicketForm-subject' || k === 'Size-supportTicketForm-subject' || k === 'NotEmpty-supportTicketForm-message' || k === 'Size-supportTicketForm-message') {
                        ACC.customerticketingaddon.addHasErrorClass();
                    } else {
                        ACC.customerticketingaddon.displayGlobalAlert({
                            type: 'error',
                            message: v
                        });
                    }
                });
            });
            return;
        }
        ACC.customerticketingaddon.displayCustomerTicketingAlert({
            type: 'error',
            messageId: 'supporttickets-tryLater'
        });
    },
    addHasErrorClass: function() {
        $('#createTicket-message').parent().addClass('has-error');
    },
    clearAlerts: function() {
        $('#customer-ticketing-alerts').empty();
        $('#global-alerts').empty();
        $('#NotEmpty-supportTicketForm-subject').hide();
        $('#Size-supportTicketForm-message').hide();
        $('#Size-supportTicketForm-subject').hide();
        $('#createTicket-subject').parent().removeClass('has-error');
        $('#NotEmpty-supportTicketForm-message').hide();
        $('#createTicket-message').parent().removeClass('has-error');
    }
};

(function() {
    var listOfActions = Array.prototype.slice.call(document.querySelectorAll('[class^="AccountOrderDetailsOverviewComponent"]'), 0);
    var firstVisibleAction = listOfActions.filter(function(element) {
        return element.childElementCount > 0;
    })[0];
    if (firstVisibleAction) {
        var target = firstVisibleAction.querySelectorAll('input[type="submit"], button')[0];
        target.className = target.className.replace(/btn\-default/g, 'btn-primary');
    }
})();

ACC.cancelorderaction = {
    _autoload: [
        ["bindToCancelCompleteOrderButton", $(".js-cancel-complete-order-link").length != 0], "bindToCancelEntryQuantityInput", "bindToCancelEntryQuantityFocusOut"
    ],
    bindToCancelCompleteOrderButton: function() {
        $(document).on('click', '.js-cancel-complete-order-link', function(event) {
            event.preventDefault();
            $.each($('[id^="item_quantity_"]'), function(i) {
                $('[name^="cancelEntryQuantityMap[' + i + ']"]').val($('#item_quantity_' + i).val())
            });
            ACC.cancelorderaction.disableEnableCancelSubmit();
        });
    },
    bindToCancelEntryQuantityInput: function() {
        $('input[id^="cancelEntryQuantityMap"]').keypress(function(e) {
            if (e.which != 8 && e.which != 0 && (e.which < 48 || e.which > 57)) {
                return false;
            }
        });
    },
    bindToCancelEntryQuantityFocusOut: function() {
        $('[name^="cancelEntryQuantityMap"]').focusout(function(field) {
            var index = this.id.replace("cancelEntryQuantityMap", "");
            if (parseInt($('#item_quantity_' + index).val()) < parseInt(this.value)) {
                this.value = $('#item_quantity_' + index).val();
            }
            $('[name^="cancelEntryQuantityMap[' + index + ']"]').val(this.value)
            ACC.cancelorderaction.disableEnableCancelSubmit();
        });
    },
    disableEnableCancelSubmit: function() {
        var submitDisabled = true;
        $.each($('[id^="item_quantity_"]'), function(i) {
            if (parseInt($('[name^="cancelEntryQuantityMap[' + i + ']"]').val()) > 0) {
                submitDisabled = false;
            }
        });
        $("#cancelOrderButtonConfirmation").prop("disabled", submitDisabled);
    }
};

ACC.returnorderaction = {
    _autoload: [
        ["bindToReturnCompleteOrderButton", $(".js-return-complete-order-link").length != 0], "bindToReturnEntryQuantityInput", "bindToReturnEntryQuantityFocusOut"
    ],
    bindToReturnCompleteOrderButton: function() {
        $(document).on('click', '.js-return-complete-order-link', function(event) {
            event.preventDefault();
            $.each($('[id^="item_quantity_"]'), function(i) {
                $('[name^="returnEntryQuantityMap[' + i + ']"]').val($('#item_quantity_' + i).val())
            });
            ACC.returnorderaction.disableEnableReturnSubmit();
        });
    },
    bindToReturnEntryQuantityInput: function() {
        $('input[id^="returnEntryQuantityMap"]').keypress(function(e) {
            if (e.which != 8 && e.which != 0 && (e.which < 48 || e.which > 57)) {
                return false;
            }
        });
    },
    bindToReturnEntryQuantityFocusOut: function() {
        $('[name^="returnEntryQuantityMap"]').focusout(function(field) {
            var index = this.id.replace("returnEntryQuantityMap", "");
            if (parseInt($('#item_quantity_' + index).val()) < parseInt(this.value)) {
                this.value = $('#item_quantity_' + index).val();
            }
            $('[name^="returnEntryQuantityMap[' + index + ']"]').val(this.value)
            ACC.returnorderaction.disableEnableReturnSubmit();
        });
    },
    disableEnableReturnSubmit: function() {
        var submitDisabled = true;
        $.each($('[id^="item_quantity_"]'), function(i) {
            if (parseInt($('[name^="returnEntryQuantityMap[' + i + ']"]').val()) > 0) {
                submitDisabled = false;
            }
        });
        $("#returnOrderButtonConfirmation").prop("disabled", submitDisabled);
    }
};

var ASM = ASM || {};
var sessionSec;
var counter;
var carts;

function addASMHandlers() {
    revertAutocompleteNormalize();
    removeAsmAlert(3000);
    addCloseBtnHandler();
    addASMFormHandler();
    addHideBtnHandler();
    addCustomerListBtnHandler();
    customerListModalHandler();
    addCustomer360Handler();
    addGenericCustomer360Handler();
    if ($("#sessionTimer").length && $('#asmLogoutForm').length) {
        startTimer();
    }
    if ($("#resetButton").length) {
        $("#resetButton").click(function() {
            resetSession();
        });
    }
    if (placeholderNotAvailable()) {
        $('[placeholder]').focus(function() {
            var input = $(this);
            if (input.val() == input.attr('placeholder')) {
                input.val('');
                input.removeClass('placeholder');
            }
        }).blur(function() {
            var input = $(this);
            if (input.val() == '' || input.val() == input.attr('placeholder')) {
                input.addClass('placeholder');
                input.val(input.attr('placeholder'));
            }
        }).blur();
    }
    $('[placeholder]').blur(function() {
        var input = $(this);
        if (input.val() == '') {
            if (input.attr("name")) {
                toggleBind(false);
            }
        }
    });
    if ($('.ASM_alert_cart').length) {
        $("input[name='cartId']").addClass('ASM-input-error');
    }
    if ($('.ASM_alert_customer').length) {
        $("input[name='customerName']").addClass('ASM-input-error');
    }
    if ($('.ASM_alert_cred').length) {
        $("input[name='username']").addClass('ASM-input-error');
        $("input[name='password']").addClass('ASM-input-error');
    }
    if ($('.ASM_alert_create_new').length) {
        toggleCreateAccount(true);
    }
    if ($('#_asmLogin').length) {
        var loginUser = $("#asmLoginForm input[name='username']");
        var min = 1;
        if (loginUser.val().length >= min) {
            loginUser.parent().addClass('checked');
        }
    }
    $("#asmLoginForm input[name='username'], #asmLoginForm input[name='password']").keyup(function() {
        var min = 1;
        var parentNode = $(this.parentNode)
        if (this.value.length >= (min)) {
            parentNode.addClass('checked');
            checkSignInButton(parentNode);
        } else {
            parentNode.removeClass('checked');
            checkSignInButton(parentNode);
        }
    });
    $("input[name='customerName']").keyup(function(e) {
        validateNewAccount(this);
        $(this).removeData("hover");
        removeAsmHover();
        toggleBind(false);
        toggleStartSessionButton(this, false);
        if ($(this).val().length < 3) {
            toggleCreateAccount(false);
        }
    });
    $("#_asmPersonifyForm input[name='cartId']").keyup(function() {
        formValidate(this, 8, true, 8);
        if (isErrorDisplayed()) {
            $("input[name='cartId']").removeClass('ASM-input-error');
            if ($('.ASM_alert')) {
                $('.ASM_alert').remove();
            }
        }
    });
    $("#_asmPersonifyForm input[name='customerName']").keyup(function() {
        if (isErrorDisplayed()) {
            $("input[name='customerName']").removeClass('ASM-input-error');
            if ($('.ASM_alert')) {
                $('.ASM_alert').remove();
            }
            if ($(this).val() == "") {
                $("input[name='cartId']").removeClass('ASM-input-error');
                toggleStartSessionButton($("input[name='cartId']"), true);
                $("input[name='customerId']").val("");
            }
        }
        if ($(this).val() == "") {
            $("input[name='cartId']").val("");
            $("#asmAutoCompleteCartId").empty();
        }
    });
    $("#_asmPersonifyForm input[name='cartId']").blur(function() {
        var regEx = /^\s+$/;
        if (regEx.test($(this).val())) {
            $(this).val('');
            formValidate(this, 8, true, 8);
        }
    });
    $("#_asmBindForm input[name='cartId']").keyup(function(e) {
        checkCartIdFieldAndToggleBind(this);
    });
    $("#_asmBindForm input[name='cartId']").bind('paste', function(e) {
        var inputField = this;
        setTimeout(function() {
            checkCartIdFieldAndToggleBind(inputField);
        }, 100);
    });
    $("#_asmPersonifyForm input[name='customerName'], input[name='customerId']").hover(function() {
        var item = ($(this).attr('data-hover')) ? jQuery.parseJSON($(this).attr('data-hover')) : $(this).data("hover");
        var disabled = ($(this).attr('data-hover')) ? "disabled" : "";
        if (!(item === null || item === undefined)) {
            $(this).after($('<div>').attr('id', 'asmHover').addClass(disabled).append($('<span>').addClass('name').text(item.name), $('<span>').addClass('email').text(item.email), $('<span>').addClass('bpNo').text(item.bpNo), $('<span>').addClass('date').text(item.date)));
        }
    }, function() {
        removeAsmHover();
    });
    $("#_asmPersonifyForm input[name='cartId']").autocomplete({
        source: function(request, response) {
            response(carts);
        },
        appendTo: "#asmAutoCompleteCartId",
        autoFocus: true,
        minLength: 0,
        select: function(event, ui) {
            if (ui.item.value == "") {
                event.preventDefault();
            }
            toggleStartSessionButton(this, true);
        }
    });
    $("#_asmPersonifyForm input[name='cartId']").on('click, focus', function() {
        $("#_asmPersonifyForm input[name='cartId']").autocomplete('search', '');
    });
    if ($("input[name='customerName']").length > 0) {
        $("input[name='customerName']").autocomplete({
            source: function(request, response) {
                $.ajax({
                    url: ACC.config.encodedContextPath + "/assisted-service/autocomplete",
                    dataType: "jsonp",
                    data: {
                        customerId: request.term
                    },
                    success: function(data) {
                        response(data);
                    }
                });
            },
            minLength: 3,
            appendTo: "#asmAutoComplete",
            select: function(event, ui) {
                if (ui.item.value == "") {
                    event.preventDefault();
                    return;
                }
                toggleStartSessionButton(this, true);
                $(this).data('hover', {
                    name: ui.item.value,
                    email: ui.item.email,
                    bpNo: ui.item.bpNo,
                    date: ui.item.date
                });
                $("input[name='customerId']").val(ui.item.email);
                carts = ui.item.carts;
                if ($("input[name='cartId']").attr("orig_value") == null) {
                    $("input[name='cartId']").val('');
                    if (carts != null) {
                        if (carts.length == 1) {
                            $("input[name='cartId']").val(carts[0]);
                        } else {
                            $("input[name='cartId']").autocomplete('search', '');
                            $("input[name='cartId']").focus();
                        }
                    } else {
                        carts = [{
                            label: "No Existing Carts",
                            value: ""
                        }];
                        $("input[name='cartId']").autocomplete('search', '');
                        $("input[name='cartId']").focus();
                    }
                }
                toggleBind(true);
            }
        }).data("ui-autocomplete")._renderItem = function(ul, item) {
            if (item.value == "") {
                toggleCreateAccount(true);
                return $("<li></li>").data("item.autocomplete", item).append($('<span class=noresult>').text(ASM_MESSAGES.customerIdNotFound)).appendTo(ul);
            } else {
                toggleCreateAccount(false);
            }
            return $("<li></li>").data("item.autocomplete", item).append($('<span>').addClass('name').text(item.value), $('<span>').addClass('email').text(item.email), $('<span>').addClass('bpNo').text(item.bpNo), $('<span>').addClass('date').text(item.date)).appendTo(ul);
        };
    }
    if ($("#_asmBindForm").length) {
        var customerId = $("input[name='customerName']").attr('readonly');
        var cartId = $(".cartId input").attr('disabled');
        if (customerId === "readonly") {
            $(".ASM_icon-chain").removeClass('invisible').addClass('ASM_chain-bind');
            if ($("#_asmBindForm input[name='customerId']").val() != undefined && $("#_asmBindForm input[name='customerId']").val() != "") {
                $(".js-customer360").removeAttr('disabled');
            }
        }
    }
    if ($(".add_to_cart_form").length && $("#_asm input[name='cartId']").val() == "") {
        $(".add_to_cart_form").submit(function(event) {
            setTimeout(function() {
                var url = ACC.config.encodedContextPath + "/assisted-service/add-to-cart";
                $.post(url, function(data) {
                    $("#_asm").replaceWith(data);
                    addASMHandlers();
                })
            }, 400);
        });
    }
    enableAsmPanelButtons();
}
$(document).ready(function() {
    var ASM = ASM || {};
    addASMHandlers();
    $(document).on("click", ".js-select-store-label", function(e) {
        $("#colorbox .js-pickup-component").addClass("show-store");
        colorboxResize();
    });
    $(document).on("click", ".js-asm-store-finder-details-back", function(e) {
        $("#colorbox .js-pickup-component").removeClass("show-store");
    })
});

function addASMFormHandler() {
    if ($) {
        if ($(".asmForm").length) {
            $(".asmForm").each(function() {
                $(this).submit(function() {
                    $(this).find('[placeholder]').each(function() {
                        var input = $(this);
                        if (input.val() == input.attr('placeholder')) {
                            input.val('');
                        }
                    })
                    $.ajax({
                        type: "POST",
                        url: $(this).attr('action'),
                        data: $(this).serialize(),
                        success: function(data) {
                            $("#_asm").replaceWith(data);
                            addASMHandlers();
                        }
                    });
                    return false;
                });
            });
        }
    }
}

function logoutSessionUser() {
    console.log("logout session user");
    var logoutUrl = ACC.config.encodedContextPath + "/logout";
    console.log(logoutUrl);
    window.location.replace(logoutUrl);
}

function addCloseBtnHandler() {
    $("#_asm .closeBtn").click(function() {
        $("#_asm").remove();
        var url = ACC.config.encodedContextPath + "/assisted-service/quit";
        $.post(url, function(data) {
            var oldurl = window.location.href;
            var newurl = oldurl.replace("&asm=true", "").replace("?asm=true&", "?").replace("?asm=true", "");
            window.location.replace(newurl);
        });
    });
}

function addHideBtnHandler() {
    $("#_asm .ASM_control_collapse").click(function() {
        $("#_asm").toggleClass("ASM-collapsed");
    });
}

function startTimer() {
    sessionSec = timer;
    clearInterval(counter);
    counter = setInterval(timerFunc, 1000);
}

function timerFunc() {
    if (sessionSec <= 0) {
        clearInterval(counter);
        finishASMagentSession();
        return;
    }
    sessionSec = sessionSec - 1;
    var min = Math.floor(sessionSec / 60);
    var sec = sessionSec % 60;
    if (min < 10) {
        min = "0" + min;
    }
    if (sec < 10) {
        sec = "0" + sec;
    }
    $("#sessionTimer .ASM_timer_count").html(min + ":" + sec);
}

function resetSession() {
    var request = $.ajax({
        url: ACC.config.encodedContextPath + "/assisted-service/resetSession",
        type: "POST"
    });
    request.done(function(msg) {
        sessionSec = timer + 1;
    });
    request.fail(function(jqXHR, textStatus) {
        $('#errors').empty();
        $('#errors').append("Request failed: " + textStatus);
    });
}

function finishASMagentSession() {
    $.ajax({
        url: ACC.config.encodedContextPath + "/assisted-service/logoutasm",
        type: "POST",
        success: function(data) {
            console.log("Logout asm");
            addASMHandlers();
            logoutSessionUser();
        }
    });
}

function isStartEmulateButtonPresent() {
    return $(".ASM-btn-start-session").length == 1;
}

function enableAsmPanelButtons() {
    $('div[id="_asm"] button').not(".js-customer360, .ASM-btn-start-session, .ASM-btn-create-account, .ASM-btn-login").removeAttr('disabled');
    if (isStartEmulateButtonPresent()) {
        if ($("#_asmPersonifyForm input[name='customerId']").val() != "") {
            $("#_asmPersonifyForm input[name='customerId']").parent().addClass("checked");
        }
        formValidate($("#_asmPersonifyForm input[name='cartId']")[0], 8, true, 8);
    }
}

function placeholderNotAvailable() {
    var i = document.createElement('input');
    return !('placeholder' in i);
}

function removeAsmHover() {
    $('#asmHover').remove();
}

function toggleCreateAccount(activate) {
    var bindIcon = $(".ASM_icon-chain");
    var createButton = $("#_asmCreateAccountForm button.ASM-btn-create-account[type='submit']");
    if (activate) {
        createButton.removeClass('hidden');
        bindIcon.removeClass('invisible');
    } else {
        createButton.addClass('hidden');
        bindIcon.addClass('invisible');
    }
}

function toggleActivationState(button, activate) {
    if (activate) {
        button.removeAttr('disabled');
    } else {
        button.attr('disabled', '');
    }
}

function checkSignInButton(el) {
    var signInBtn = $("#asmLoginForm button[type='submit']");
    var checkSum = $(el).parent().find('.checked').length;
    if (checkSum > 1) {
        toggleActivationState(signInBtn, true);
    } else {
        toggleActivationState(signInBtn, false);
    }
}

function checkStartSessionButton(el) {
    toggleStartSessionButton(el, false);
    var checkSum = $(el.parentNode).siblings('.checked').length;
    if (checkSum > 0) {
        toggleActivationState($("button.ASM-btn-start-session"), true);
    }
}

function checkCartIdFieldAndToggleBind(cartIdField) {
    if (!$(cartIdField).hasClass('placeholder') && $("input[name='customerId']").val().length > 0) {
        if (!isNaN(cartIdField.value) && (cartIdField.value.length == 8) && (cartIdField.value != cartIdField.getAttribute('orig_value'))) {
            $("#_asmBindForm button[type='submit']").removeClass('hidden');
            $(".ASM_icon-chain").removeClass('invisible');
            return;
        }
    }
    $("#_asmBindForm button[type='submit']").addClass('hidden');
    $(".ASM_icon-chain").addClass('invisible');
}

function toggleBind(activate) {
    if ($("#_asmBindForm").length) {
        var bindIcon = $(".ASM_icon-chain");
        var bindButton = $("#_asmBindForm button.ASM-btn-bind-cart[type='submit']");
        if (activate) {
            bindButton.removeClass('hidden');
            bindIcon.removeClass('invisible');
        } else {
            bindButton.addClass('hidden');
            if ($('.ASM-btn-create-account').hasClass('hidden')) {
                bindIcon.addClass('invisible');
            }
        }
    }
}

function toggleStartSessionButton(el, activate) {
    var checkedItem = $(el).parent();
    var button = $("button.ASM-btn-start-session");
    if (activate) {
        button.removeAttr('disabled');
        checkedItem.addClass("checked");
    } else {
        button.attr('disabled', '');
        checkedItem.removeClass("checked");
    }
}

function formValidate(el, min, number, max) {
    if (!$(el).hasClass('placeholder')) {
        if ($(el).hasClass("ASM-input-error")) {
            toggleStartSessionButton(el, false);
            return false;
        }
        if (number !== false) {
            if (isNaN(el.value)) {
                toggleStartSessionButton(el, false);
                return false;
            }
        }
        if (el.value.length >= (min)) {
            toggleStartSessionButton(el, true);
            if (max !== undefined && el.value.length > (max)) {
                toggleStartSessionButton(el, false);
            }
        } else if (el.value.length === 0) {
            checkStartSessionButton(el);
        } else {
            toggleStartSessionButton(el, false);
            return false;
        }
        return true;
    }
    return false;
}

function validateEmail(mailAddress) {
    return ($('<input>').attr({
        type: 'email',
        required: 'required'
    }).val(mailAddress))[0].checkValidity() && (mailAddress.indexOf(".") > 0);
}

function validateName(name) {
    var regEx = /^[a-zA-Z-]+\s[a-zA-Z-]+$/;
    return (name != '' && regEx.test(name));
}

function validateNewAccount(el) {
    var createAccountButton = $("#_asmCreateAccountForm button.ASM-btn-create-account[type='submit']");
    var customerValues = el.value.split(', ');
    var IdInput = $("#_asmCreateAccountForm input[name='customerId']");
    var NameInput = $("#_asmCreateAccountForm input[name='customerName']");
    if (customerValues.length > 1) {
        var validName = validateName(customerValues[0]);
        var validMail = validateEmail(customerValues[1]);
        if (validName && validMail) {
            toggleActivationState(createAccountButton, true);
            IdInput.val(customerValues[1].replace(/^\s\s*/, '').replace(/\s\s*$/, ''));
            NameInput.val(customerValues[0]);
        } else {
            toggleActivationState(createAccountButton, false);
            return false;
        }
    } else {
        toggleActivationState(createAccountButton, false);
        return false;
    }
}

function revertAutocompleteNormalize() {
    $.ui.autocomplete.prototype._normalize = function(a) {
        if (a.length && a[0].label && a[0].value) {
            return a;
        }
        return $.map(a, function(b) {
            if (typeof b === "string") {
                return {
                    label: b,
                    value: b
                };
            }
            return $.extend({
                label: b.label || b.value,
                value: b.value || b.label
            }, b);
        });
    };
}

function isErrorDisplayed() {
    return $('.ASM_alert').length;
}

function addCustomerListBtnHandler() {
    $(".js-customer-list-btn").removeClass('disabled');
    $(document).on("click", ".js-customer-list-btn", function(e) {
        e.preventDefault();
        populateCustomerListModal($(this).data('actionurl'), '.js-customer-list-modal-content', addCustomerListSelect);
    });
}

function openCustomer360Colorbox(colorboxTarget) {
    colorboxTarget.colorbox({
        inline: 'true',
        className: 'ASM_customer360-modal',
        width: "100%",
        maxWidth: "1200px",
        close: '<span class="ASM_icon ASM_icon-close"></span>',
        transition: 'none',
        scrolling: false,
        opacity: 0.7,
        top: 10,
        onOpen: function() {
            customer360Callback();
            $(window).on("resize", colorboxResize);
        },
        onClosed: function() {
            $(window).off("resize", colorboxResize);
        }
    });
}

function colorboxResize() {
    $.colorbox.resize()
}

function addCustomer360Handler() {
    openCustomer360Colorbox($(".js-customer360"));
}

function addGenericCustomer360Handler() {
    if ($("#enable360View").val()) {
        openCustomer360Colorbox($);
    }
}

function customer360Callback() {
    var loader = "<div class='loader'>Loading..</div>";
    $("#cboxLoadedContent").html(loader).show();
    $.ajax({
        url: ACC.config.encodedContextPath + "/assisted-service-aif/customer360",
        type: "GET",
        success: function(data) {
            $("#cboxLoadedContent").append(data);
            $.colorbox.resize();
        },
        error: function(xht, textStatus, ex) {
            console.error("Failed to load Customer 360. %s", ex);
            document.location.reload();
        }
    });
}

function loadCustomer360Fragment(params) {
    return $.ajax({
        url: ACC.config.encodedContextPath + "/assisted-service-aif/customer360Fragment",
        timeout: params.timeout,
        type: params.method,
        data: params,
        success: function(data) {
            $("#" + params.fragmentId).html(data);
            $.colorbox.resize();
        },
        error: function(xht, textStatus, ex) {
            if (textStatus === 'timeout') {
                $("#" + params.fragmentId).html("Widget timeout!");
            } else {
                console.error("Failed to get widget data! %s", ex);
                $("#" + params.fragmentId).html("Failed to get widget data!");
            }
        }
    });
}

function asmAifSectionClickHandler() {
    $(document).on("click", ".asm__customer360__menu li", function(e) {
        e.preventDefault();
        if (!$(this).hasClass('nav-tabs-mobile-caret')) {
            aifSelectSection($(this).index());
        }
    });
};

function aifSelectLastSection() {
    var index = 0;
    if (sessionStorage.getItem("lastSection")) {
        var lastSection = JSON.parse(sessionStorage.getItem("lastSection"));
        if (getCurrentEmulatedCustomerId() == lastSection.userId) {
            index = lastSection.sectionId;
        }
    }
    $($(".asm__customer360__menu li[role='presentation']")[index]).addClass("active");
    aifSelectSection(index);
}

function aifSelectSection(index) {
    $("#sectionPlaceholder").hide();
    $("#longLoadExample").show();
    var sectionId = $(".asm__customer360__menu li").get(index).getAttribute("value");
    sessionStorage.setItem("lastSection", JSON.stringify({
        userId: getCurrentEmulatedCustomerId(),
        sectionId: index
    }));
    $.ajax({
        url: ACC.config.encodedContextPath + "/assisted-service-aif/customer360section?sectionId=" + sectionId,
        type: "GET",
        success: function(data) {
            $("#sectionPlaceholder").html(data);
            $("#longLoadExample").hide();
            $("#sectionPlaceholder").show();
            $.colorbox.resize();
        }
    });
    resetSession();
}

function getCurrentEmulatedCustomerId() {
    if ($("#_asmBindForm input[name='customerId']").length) {
        return $("#_asmBindForm input[name='customerId']").val();
    }
    return "anonymous";
}

function getCustomerListSearchUrl() {
    var targetUrl = $(".js-customer-list-sorting").data('sort-url');
    targetUrl += $(".ASM_customer-list-modal .sort-refine-bar .form-control").val();
    var query = $("#ASM_customer-list-queryInput").val();
    var uriEncodedquery = encodeURIComponent(query);
    targetUrl += '&query=' + uriEncodedquery;
    return targetUrl;
}

function customerListModalHandler() {
    $(document).on("click", ".ASM_customer-list-modal .pagination a", function(e) {
        e.preventDefault();
        populateCustomerListModal($(this).attr('href'), ".asm-account-section", replaceCustomerListTable);
    });
    $(document).on("click", "#ASM_customer-list-sortOptions .sortOption", function(e) {
        e.preventDefault();
        var selectedOption = $(this).data('value');
        var previouslySelectedOption = $(".ASM_customer-list-modal .sort-refine-bar .form-control").val();
        if (selectedOption != previouslySelectedOption) {
            $(".ASM_customer-list-modal .sort-refine-bar .form-control").val(selectedOption);
            var targetUrl = getCustomerListSearchUrl();
            populateCustomerListModal(targetUrl, ".asm-account-section", replaceCustomerListTable);
        }
    });
    $(document).on("keypress", "#ASM_customer-list-queryInput", function(event) {
        if (event.keyCode == 13) {
            $("#ASM_customer-list-searchButton").click();
            return false;
        } else {
            return true;
        }
    });
    $(document).on("click", "#ASM_customer-list-searchButton", function(e) {
        e.preventDefault();
        var targetUrl = getCustomerListSearchUrl()
        populateCustomerListModal(targetUrl, ".asm-account-section", replaceCustomerListTable);
    });
    $(document).on("change", ".ASM_customer-list-modal .sort-refine-bar .form-control", function(e) {
        e.preventDefault();
        var targetUrl = getCustomerListSearchUrl();
        populateCustomerListModal(targetUrl, ".asm-account-section", replaceCustomerListTable);
    });
    $(document).on("change", ".js-customer-list-select", function(e) {
        e.preventDefault();
        var targetUrl = $(this).data('search-url');
        targetUrl += $(this).val();
        var request = populateCustomerListModal(targetUrl, ".asm-account-section", replaceCustomerListTable);
        request.done(function() {
            $.colorbox.resize();
        });
    });
}

function addCustomerListSelect(componentToUpdate, data) {
    var selector = $(data).find('.js-customer-list-select');
    $(componentToUpdate).html(data);
    searchUrl = $(data).find('.js-customer-list-select').data('search-url');
    if (selector[0].options.length > 0) {
        searchUrl += selector[0].options[0].value;
    }
    var request = populateCustomerListModal(searchUrl, componentToUpdate, appendCustomerListTable);
    request.done(function() {
        ACC.colorbox.open("", {
            href: ".js-customer-list-modal-content",
            inline: true,
            className: 'ASM_customer-list-modal',
            width: "100%",
            maxWidth: "1200px",
            close: '<span class="ASM_icon ASM_icon-close"></span>',
            transition: 'none',
            scrolling: false,
            opacity: 0.7,
            top: 10,
            onOpen: function() {
                $(window).on("resize", colorboxResize);
            },
            onClosed: function() {
                $(window).off("resize", colorboxResize);
            }
        });
    });
}

function appendCustomerListTable(componentToUpdate, data) {
    $(componentToUpdate).append(data);
}

function replaceCustomerListTable(componentToUpdate, data) {
    $(componentToUpdate).html(data);
}

function populateCustomerListModal(targetUrl, componentToUpdate, callFunction) {
    var method = "GET";
    return $.ajax({
        url: targetUrl,
        type: method,
        success: function(data) {
            callFunction(componentToUpdate, data);
        },
        error: function(xht, textStatus, ex) {
            console.error("Failed to get customer list. %s", ex);
            document.location.reload();
        }
    });
};

function getAifTablePageSize() {
    var pagesNumber = 5;
    if ($(window).width() < 668) {
        pagesNumber = 10;
    }
    return pagesNumber;
}

function copyToClipBoard(text) {
    $("#asmCopyHoldtext").val(text);
    $("#asmCopyHoldtext").show();
    $("#asmCopyHoldtext").select();
    try {
        return document.execCommand("copy");
    } catch (ex) {
        console.warn("Copy to clipboard failed.", ex);
        return false;
    } finally {
        $("#asmCopyHoldtext").hide();
    }
}

function addRatesTableSorterParser() {
    $.tablesorter.addParser({
        id: 'rates',
        is: function(s) {
            return false;
        },
        format: function(s, table, cell) {
            return Math.floor($(cell).attr('data-text') * 10);
        },
        type: 'numeric'
    });
}

function removeAsmAlert(delay) {
    setTimeout(function() {
        $(".ASM_alert").fadeOut("slow");
    }, delay);
};
ACC.makasmstorefrontaddon = {
    drawDoughnutChart: function(id, labels, values) {
        console.log("Labels", labels);
        var ctx = document.getElementById(id);
        var chart = new Chart(ctx, {
            type: 'doughnut',
            data: {
                labels: labels,
                datasets: [{
                    data: values,
                    backgroundColor: ["#61A9DC", "#5F7084", "#DB6580", "#E89C17"],
                    borderWidth: 1
                }]
            },
            options: {
                animation: false,
                responsive: true,
                legend: {
                    position: "right"
                },
                showLabel: true,
                tooltips: {
                    backgroundColor: "#7F90A4",
                    yAlign: "bottom"
                }
            }
        });
    },
    drawPolarAreaChart: function(id, colorLabels, colorCodes, values) {
        var ctx = document.getElementById(id);
        var chart = new Chart(ctx, {
            type: 'polarArea',
            data: {
                labels: colorLabels,
                datasets: [{
                    data: values,
                    backgroundColor: colorCodes,
                    borderWidth: 1
                }]
            },
            options: {
                animation: false,
                responsive: true,
                legend: {
                    position: "right"
                },
                tooltips: {
                    backgroundColor: "#7F90A4",
                    yAlign: "bottom"
                },
                scale: {
                    ticks: {
                        display: false
                    }
                }
            }
        });
    },
    buildArrayValues: function(variableArray, value) {
        variableArray.push(value);
        return variableArray;
    }
};

(function($) {
    $.extend({
        tablesorterPager: new function() {
            function updatePageDisplay(c) {
                var s = $(c.cssPageDisplay, c.container).val((c.page + 1) + c.seperator + c.totalPages);
            }

            function moveToPage(table) {
                var c = table.config;
                if (c.page < 0 || c.page > (c.totalPages - 1)) {
                    c.page = 0;
                }
                renderTable(table, c.rowsCopy);
            }

            function renderTable(table, rows) {
                var c = table.config;
                var l = rows.length;
                var s = (c.page * c.size);
                var e = (s + c.size);
                if (e > rows.length) {
                    e = rows.length;
                }
                var tableBody = $(table.tBodies[0]);
                $.tablesorter.clearTableBody(table);
                for (var i = s; i < e; i++) {
                    var o = rows[i];
                    var l = o.length;
                    for (var j = 0; j < l; j++) {
                        tableBody[0].appendChild(o[j]);
                    }
                }
                $(table).trigger("applyWidgets");
                if (c.page >= c.totalPages) {
                    moveToLastPage(table);
                }
                updatePageDisplay(c);
            }
            this.appender = function(table, rows) {
                var c = table.config;
                c.rowsCopy = rows;
                c.totalRows = rows.length;
                c.totalPages = Math.ceil(c.totalRows / c.size);
                renderTable(table, rows);
            };
            this.defaults = {
                size: 5,
                offset: 0,
                page: 0,
                totalRows: 0,
                totalPages: 0,
                container: null,
                seperator: "/",
                appender: this.appender
            };
            this.construct = function(settings) {
                return this.each(function() {
                    config = $.extend(this.config, $.tablesorterPager.defaults, settings);
                    var table = this,
                        pager = config.container;
                    var curr = 0;
                    var numPages = Math.ceil($(table).find("tbody tr").length / config.size);
                    if (numPages > 1) {
                        while (numPages > curr) {
                            $('<li><a href="#" class="page_link">' + (curr + 1) + '</a></li>').appendTo(pager);
                            curr++;
                        }
                        $(this).trigger("appendCache");
                        $(pager).find('.page_link:first').addClass('active');
                        $(pager).find(".page_link").click(function() {
                            var clickedPage = $(this).html().valueOf() - 1;
                            table.config.page = clickedPage;
                            moveToPage(table);
                            pager.find(".page_link").removeClass("active");
                            pager.find(".page_link").eq(clickedPage).addClass("active");
                            return false;
                        });
                    }
                });
            };
        }
    });
    $.fn.extend({
        tablesorterPager: $.tablesorterPager.construct
    });
})(jQuery);

(function($) {
    $.extend({
        tablesorter: new
        function() {
            var parsers = [],
                widgets = [];
            this.defaults = {
                cssHeader: "header",
                cssAsc: "headerSortUp",
                cssDesc: "headerSortDown",
                cssChildRow: "expand-child",
                sortInitialOrder: "asc",
                sortMultiSortKey: "shiftKey",
                sortForce: null,
                sortAppend: null,
                sortLocaleCompare: true,
                textExtraction: "simple",
                parsers: {},
                widgets: [],
                widgetZebra: {
                    css: ["even", "odd"]
                },
                headers: {},
                widthFixed: false,
                cancelSelection: true,
                sortList: [],
                headerList: [],
                dateFormat: "us",
                decimal: '/\.|\,/g',
                onRenderHeader: null,
                selectorHeaders: 'thead th',
                debug: false
            };

            function benchmark(s, d) {
                log(s + "," + (new Date().getTime() - d.getTime()) + "ms");
            }
            this.benchmark = benchmark;

            function log(s) {
                if (typeof console != "undefined" && typeof console.debug != "undefined") {
                    console.log(s);
                } else {
                    alert(s);
                }
            }

            function buildParserCache(table, $headers) {
                if (table.config.debug) {
                    var parsersDebug = "";
                }
                if (table.tBodies.length == 0) return;
                var rows = table.tBodies[0].rows;
                if (rows[0]) {
                    var list = [],
                        cells = rows[0].cells,
                        l = cells.length;
                    for (var i = 0; i < l; i++) {
                        var p = false;
                        if ($.metadata && ($($headers[i]).metadata() && $($headers[i]).metadata().sorter)) {
                            p = getParserById($($headers[i]).metadata().sorter);
                        } else if ((table.config.headers[i] && table.config.headers[i].sorter)) {
                            p = getParserById(table.config.headers[i].sorter);
                        }
                        if (!p) {
                            p = detectParserForColumn(table, rows, -1, i);
                        }
                        if (table.config.debug) {
                            parsersDebug += "column:" + i + " parser:" + p.id + "\n";
                        }
                        list.push(p);
                    }
                }
                if (table.config.debug) {
                    log(parsersDebug);
                }
                return list;
            };

            function detectParserForColumn(table, rows, rowIndex, cellIndex) {
                var l = parsers.length,
                    node = false,
                    nodeValue = false,
                    keepLooking = true;
                while (nodeValue == '' && keepLooking) {
                    rowIndex++;
                    if (rows[rowIndex]) {
                        node = getNodeFromRowAndCellIndex(rows, rowIndex, cellIndex);
                        nodeValue = trimAndGetNodeText(table.config, node);
                        if (table.config.debug) {
                            log('Checking if value was empty on row:' + rowIndex);
                        }
                    } else {
                        keepLooking = false;
                    }
                }
                for (var i = 1; i < l; i++) {
                    if (parsers[i].is(nodeValue, table, node)) {
                        return parsers[i];
                    }
                }
                return parsers[0];
            }

            function getNodeFromRowAndCellIndex(rows, rowIndex, cellIndex) {
                return rows[rowIndex].cells[cellIndex];
            }

            function trimAndGetNodeText(config, node) {
                return $.trim(getElementText(config, node));
            }

            function getParserById(name) {
                var l = parsers.length;
                for (var i = 0; i < l; i++) {
                    if (parsers[i].id.toLowerCase() == name.toLowerCase()) {
                        return parsers[i];
                    }
                }
                return false;
            }

            function buildCache(table) {
                if (table.config.debug) {
                    var cacheTime = new Date();
                }
                var totalRows = (table.tBodies[0] && table.tBodies[0].rows.length) || 0,
                    totalCells = (table.tBodies[0].rows[0] && table.tBodies[0].rows[0].cells.length) || 0,
                    parsers = table.config.parsers,
                    cache = {
                        row: [],
                        normalized: []
                    };
                for (var i = 0; i < totalRows; ++i) {
                    var c = $(table.tBodies[0].rows[i]),
                        cols = [];
                    if (c.hasClass(table.config.cssChildRow)) {
                        cache.row[cache.row.length - 1] = cache.row[cache.row.length - 1].add(c);
                        continue;
                    }
                    cache.row.push(c);
                    for (var j = 0; j < totalCells; ++j) {
                        cols.push(parsers[j].format(getElementText(table.config, c[0].cells[j]), table, c[0].cells[j]));
                    }
                    cols.push(cache.normalized.length);
                    cache.normalized.push(cols);
                    cols = null;
                };
                if (table.config.debug) {
                    benchmark("Building cache for " + totalRows + " rows:", cacheTime);
                }
                return cache;
            };

            function getElementText(config, node) {
                var text = "";
                if (!node) return "";
                if (!config.supportsTextContent) config.supportsTextContent = node.textContent || false;
                if (config.textExtraction == "simple") {
                    if (config.supportsTextContent) {
                        text = node.textContent;
                    } else {
                        if (node.childNodes[0] && node.childNodes[0].hasChildNodes()) {
                            text = node.childNodes[0].innerHTML;
                        } else {
                            text = node.innerHTML;
                        }
                    }
                } else {
                    if (typeof(config.textExtraction) == "function") {
                        text = config.textExtraction(node);
                    } else {
                        text = $(node).text();
                    }
                }
                return text;
            }

            function appendToTable(table, cache) {
                if (table.config.debug) {
                    var appendTime = new Date()
                }
                var c = cache,
                    r = c.row,
                    n = c.normalized,
                    totalRows = n.length,
                    checkCell = (n[0].length - 1),
                    tableBody = $(table.tBodies[0]),
                    rows = [];
                for (var i = 0; i < totalRows; i++) {
                    var pos = n[i][checkCell];
                    rows.push(r[pos]);
                    if (!table.config.appender) {
                        var l = r[pos].length;
                        for (var j = 0; j < l; j++) {
                            tableBody[0].appendChild(r[pos][j]);
                        }
                    }
                }
                if (table.config.appender) {
                    table.config.appender(table, rows);
                }
                rows = null;
                if (table.config.debug) {
                    benchmark("Rebuilt table:", appendTime);
                }
                applyWidget(table);
                setTimeout(function() {
                    $(table).trigger("sortEnd");
                }, 0);
            };

            function buildHeaders(table) {
                if (table.config.debug) {
                    var time = new Date();
                }
                var meta = ($.metadata) ? true : false;
                var header_index = computeTableHeaderCellIndexes(table);
                $tableHeaders = $(table.config.selectorHeaders, table).each(function(index) {
                    this.column = header_index[this.parentNode.rowIndex + "-" + this.cellIndex];
                    this.order = formatSortingOrder(table.config.sortInitialOrder);
                    this.count = this.order;
                    if (checkHeaderMetadata(this) || checkHeaderOptions(table, index)) this.sortDisabled = true;
                    if (checkHeaderOptionsSortingLocked(table, index)) this.order = this.lockedOrder = checkHeaderOptionsSortingLocked(table, index);
                    if (!this.sortDisabled) {
                        var $th = $(this).addClass(table.config.cssHeader);
                        if (table.config.onRenderHeader) table.config.onRenderHeader.apply($th);
                    }
                    table.config.headerList[index] = this;
                });
                if (table.config.debug) {
                    benchmark("Built headers:", time);
                    log($tableHeaders);
                }
                return $tableHeaders;
            };

            function computeTableHeaderCellIndexes(t) {
                var matrix = [];
                var lookup = {};
                var thead = t.getElementsByTagName('THEAD')[0];
                var trs = thead.getElementsByTagName('TR');
                for (var i = 0; i < trs.length; i++) {
                    var cells = trs[i].cells;
                    for (var j = 0; j < cells.length; j++) {
                        var c = cells[j];
                        var rowIndex = c.parentNode.rowIndex;
                        var cellId = rowIndex + "-" + c.cellIndex;
                        var rowSpan = c.rowSpan || 1;
                        var colSpan = c.colSpan || 1
                        var firstAvailCol;
                        if (typeof(matrix[rowIndex]) == "undefined") {
                            matrix[rowIndex] = [];
                        }
                        for (var k = 0; k < matrix[rowIndex].length + 1; k++) {
                            if (typeof(matrix[rowIndex][k]) == "undefined") {
                                firstAvailCol = k;
                                break;
                            }
                        }
                        lookup[cellId] = firstAvailCol;
                        for (var k = rowIndex; k < rowIndex + rowSpan; k++) {
                            if (typeof(matrix[k]) == "undefined") {
                                matrix[k] = [];
                            }
                            var matrixrow = matrix[k];
                            for (var l = firstAvailCol; l < firstAvailCol + colSpan; l++) {
                                matrixrow[l] = "x";
                            }
                        }
                    }
                }
                return lookup;
            }

            function checkCellColSpan(table, rows, row) {
                var arr = [],
                    r = table.tHead.rows,
                    c = r[row].cells;
                for (var i = 0; i < c.length; i++) {
                    var cell = c[i];
                    if (cell.colSpan > 1) {
                        arr = arr.concat(checkCellColSpan(table, headerArr, row++));
                    } else {
                        if (table.tHead.length == 1 || (cell.rowSpan > 1 || !r[row + 1])) {
                            arr.push(cell);
                        }
                    }
                }
                return arr;
            };

            function checkHeaderMetadata(cell) {
                if (($.metadata) && ($(cell).metadata().sorter === false)) {
                    return true;
                };
                return false;
            }

            function checkHeaderOptions(table, i) {
                if ((table.config.headers[i]) && (table.config.headers[i].sorter === false)) {
                    return true;
                };
                return false;
            }

            function checkHeaderOptionsSortingLocked(table, i) {
                if ((table.config.headers[i]) && (table.config.headers[i].lockedOrder)) return table.config.headers[i].lockedOrder;
                return false;
            }

            function applyWidget(table) {
                var c = table.config.widgets;
                var l = c.length;
                for (var i = 0; i < l; i++) {
                    getWidgetById(c[i]).format(table);
                }
            }

            function getWidgetById(name) {
                var l = widgets.length;
                for (var i = 0; i < l; i++) {
                    if (widgets[i].id.toLowerCase() == name.toLowerCase()) {
                        return widgets[i];
                    }
                }
            };

            function formatSortingOrder(v) {
                if (typeof(v) != "Number") {
                    return (v.toLowerCase() == "desc") ? 1 : 0;
                } else {
                    return (v == 1) ? 1 : 0;
                }
            }

            function isValueInArray(v, a) {
                var l = a.length;
                for (var i = 0; i < l; i++) {
                    if (a[i][0] == v) {
                        return true;
                    }
                }
                return false;
            }

            function setHeadersCss(table, $headers, list, css) {
                $headers.removeClass(css[0]).removeClass(css[1]);
                var h = [];
                $headers.each(function(offset) {
                    if (!this.sortDisabled) {
                        h[this.column] = $(this);
                    }
                });
                var l = list.length;
                for (var i = 0; i < l; i++) {
                    h[list[i][0]].addClass(css[list[i][1]]);
                }
            }

            function fixColumnWidth(table, $headers) {
                var c = table.config;
                if (c.widthFixed) {
                    var colgroup = $('<colgroup>');
                    $("tr:first td", table.tBodies[0]).each(function() {
                        colgroup.append($('<col>').css('width', $(this).width()));
                    });
                    $(table).prepend(colgroup);
                };
            }

            function updateHeaderSortCount(table, sortList) {
                var c = table.config,
                    l = sortList.length;
                for (var i = 0; i < l; i++) {
                    var s = sortList[i],
                        o = c.headerList[s[0]];
                    o.count = s[1];
                    o.count++;
                }
            }

            function multisort(table, sortList, cache) {
                if (table.config.debug) {
                    var sortTime = new Date();
                }
                var dynamicExp = "var sortWrapper = function(a,b) {",
                    l = sortList.length;
                for (var i = 0; i < l; i++) {
                    var c = sortList[i][0];
                    var order = sortList[i][1];
                    var s = (table.config.parsers[c].type == "text") ? ((order == 0) ? makeSortFunction("text", "asc", c) : makeSortFunction("text", "desc", c)) : ((order == 0) ? makeSortFunction("numeric", "asc", c) : makeSortFunction("numeric", "desc", c));
                    var e = "e" + i;
                    dynamicExp += "var " + e + " = " + s;
                    dynamicExp += "if(" + e + ") { return " + e + "; } ";
                    dynamicExp += "else { ";
                }
                var orgOrderCol = cache.normalized[0].length - 1;
                dynamicExp += "return a[" + orgOrderCol + "]-b[" + orgOrderCol + "];";
                for (var i = 0; i < l; i++) {
                    dynamicExp += "}; ";
                }
                dynamicExp += "return 0; ";
                dynamicExp += "}; ";
                if (table.config.debug) {
                    benchmark("Evaling expression:" + dynamicExp, new Date());
                }
                eval(dynamicExp);
                cache.normalized.sort(sortWrapper);
                if (table.config.debug) {
                    benchmark("Sorting on " + sortList.toString() + " and dir " + order + " time:", sortTime);
                }
                return cache;
            };

            function makeSortFunction(type, direction, index) {
                var a = "a[" + index + "]",
                    b = "b[" + index + "]";
                if (type == 'text' && direction == 'asc') {
                    return "(" + a + " == " + b + " ? 0 : (" + a + " === null ? Number.POSITIVE_INFINITY : (" + b + " === null ? Number.NEGATIVE_INFINITY : (" + a + " < " + b + ") ? -1 : 1 )));";
                } else if (type == 'text' && direction == 'desc') {
                    return "(" + a + " == " + b + " ? 0 : (" + a + " === null ? Number.POSITIVE_INFINITY : (" + b + " === null ? Number.NEGATIVE_INFINITY : (" + b + " < " + a + ") ? -1 : 1 )));";
                } else if (type == 'numeric' && direction == 'asc') {
                    return "(" + a + " === null && " + b + " === null) ? 0 :(" + a + " === null ? Number.POSITIVE_INFINITY : (" + b + " === null ? Number.NEGATIVE_INFINITY : " + a + " - " + b + "));";
                } else if (type == 'numeric' && direction == 'desc') {
                    return "(" + a + " === null && " + b + " === null) ? 0 :(" + a + " === null ? Number.POSITIVE_INFINITY : (" + b + " === null ? Number.NEGATIVE_INFINITY : " + b + " - " + a + "));";
                }
            };

            function makeSortText(i) {
                return "((a[" + i + "] < b[" + i + "]) ? -1 : ((a[" + i + "] > b[" + i + "]) ? 1 : 0));";
            };

            function makeSortTextDesc(i) {
                return "((b[" + i + "] < a[" + i + "]) ? -1 : ((b[" + i + "] > a[" + i + "]) ? 1 : 0));";
            };

            function makeSortNumeric(i) {
                return "a[" + i + "]-b[" + i + "];";
            };

            function makeSortNumericDesc(i) {
                return "b[" + i + "]-a[" + i + "];";
            };

            function sortText(a, b) {
                if (table.config.sortLocaleCompare) return a.localeCompare(b);
                return ((a < b) ? -1 : ((a > b) ? 1 : 0));
            };

            function sortTextDesc(a, b) {
                if (table.config.sortLocaleCompare) return b.localeCompare(a);
                return ((b < a) ? -1 : ((b > a) ? 1 : 0));
            };

            function sortNumeric(a, b) {
                return a - b;
            };

            function sortNumericDesc(a, b) {
                return b - a;
            };

            function getCachedSortType(parsers, i) {
                return parsers[i].type;
            };
            this.construct = function(settings) {
                return this.each(function() {
                    if (!this.tHead || !this.tBodies) return;
                    var $this, $document, $headers, cache, config, shiftDown = 0,
                        sortOrder;
                    this.config = {};
                    config = $.extend(this.config, $.tablesorter.defaults, settings);
                    $this = $(this);
                    $.data(this, "tablesorter", config);
                    $headers = buildHeaders(this);
                    this.config.parsers = buildParserCache(this, $headers);
                    cache = buildCache(this);
                    var sortCSS = [config.cssDesc, config.cssAsc];
                    fixColumnWidth(this);
                    $headers.click(function(e) {
                        var totalRows = ($this[0].tBodies[0] && $this[0].tBodies[0].rows.length) || 0;
                        if (!this.sortDisabled && totalRows > 0) {
                            $this.trigger("sortStart");
                            var $cell = $(this);
                            var i = this.column;
                            this.order = this.count++ % 2;
                            if (this.lockedOrder) this.order = this.lockedOrder;
                            if (!e[config.sortMultiSortKey]) {
                                config.sortList = [];
                                if (config.sortForce != null) {
                                    var a = config.sortForce;
                                    for (var j = 0; j < a.length; j++) {
                                        if (a[j][0] != i) {
                                            config.sortList.push(a[j]);
                                        }
                                    }
                                }
                                config.sortList.push([i, this.order]);
                            } else {
                                if (isValueInArray(i, config.sortList)) {
                                    for (var j = 0; j < config.sortList.length; j++) {
                                        var s = config.sortList[j],
                                            o = config.headerList[s[0]];
                                        if (s[0] == i) {
                                            o.count = s[1];
                                            o.count++;
                                            s[1] = o.count % 2;
                                        }
                                    }
                                } else {
                                    config.sortList.push([i, this.order]);
                                }
                            };
                            setTimeout(function() {
                                setHeadersCss($this[0], $headers, config.sortList, sortCSS);
                                appendToTable($this[0], multisort($this[0], config.sortList, cache));
                            }, 1);
                            return false;
                        }
                    }).mousedown(function() {
                        if (config.cancelSelection) {
                            this.onselectstart = function() {
                                return false
                            };
                            return false;
                        }
                    });
                    $this.bind("update", function() {
                        var me = this;
                        setTimeout(function() {
                            me.config.parsers = buildParserCache(me, $headers);
                            cache = buildCache(me);
                        }, 1);
                    }).bind("updateCell", function(e, cell) {
                        var config = this.config;
                        var pos = [(cell.parentNode.rowIndex - 1), cell.cellIndex];
                        cache.normalized[pos[0]][pos[1]] = config.parsers[pos[1]].format(getElementText(config, cell), cell);
                    }).bind("sorton", function(e, list) {
                        $(this).trigger("sortStart");
                        config.sortList = list;
                        var sortList = config.sortList;
                        updateHeaderSortCount(this, sortList);
                        setHeadersCss(this, $headers, sortList, sortCSS);
                        appendToTable(this, multisort(this, sortList, cache));
                    }).bind("appendCache", function() {
                        appendToTable(this, cache);
                    }).bind("applyWidgetId", function(e, id) {
                        getWidgetById(id).format(this);
                    }).bind("applyWidgets", function() {
                        applyWidget(this);
                    });
                    if ($.metadata && ($(this).metadata() && $(this).metadata().sortlist)) {
                        config.sortList = $(this).metadata().sortlist;
                    }
                    if (config.sortList.length > 0) {
                        $this.trigger("sorton", [config.sortList]);
                    }
                    applyWidget(this);
                });
            };
            this.addParser = function(parser) {
                var l = parsers.length,
                    a = true;
                for (var i = 0; i < l; i++) {
                    if (parsers[i].id.toLowerCase() == parser.id.toLowerCase()) {
                        a = false;
                    }
                }
                if (a) {
                    parsers.push(parser);
                };
            };
            this.addWidget = function(widget) {
                widgets.push(widget);
            };
            this.formatFloat = function(s) {
                var i = parseFloat(s);
                return (isNaN(i)) ? 0 : i;
            };
            this.formatInt = function(s) {
                var i = parseInt(s);
                return (isNaN(i)) ? 0 : i;
            };
            this.isDigit = function(s, config) {
                return /^[-+]?\d*$/.test($.trim(s.replace(/[,.']/g, '')));
            };
            this.clearTableBody = function(table) {
                if (navigator.userAgent.match("MSIE")) {
                    function empty() {
                        while (this.firstChild) this.removeChild(this.firstChild);
                    }
                    empty.apply(table.tBodies[0]);
                } else {
                    table.tBodies[0].innerHTML = "";
                }
            };
        }
    });
    $.fn.extend({
        tablesorter: $.tablesorter.construct
    });
    var ts = $.tablesorter;
    ts.addParser({
        id: "text",
        is: function(s) {
            return true;
        },
        format: function(s) {
            return $.trim(s.toLocaleLowerCase());
        },
        type: "text"
    });
    ts.addParser({
        id: "digit",
        is: function(s, table) {
            var c = table.config;
            return $.tablesorter.isDigit(s, c);
        },
        format: function(s) {
            return $.tablesorter.formatFloat(s);
        },
        type: "numeric"
    });
    ts.addParser({
        id: "currency",
        is: function(s) {
            return /^[£$€?.]/.test(s);
        },
        format: function(s) {
            return $.tablesorter.formatFloat(s.replace(new RegExp(/[£$€]/g), ""));
        },
        type: "numeric"
    });
    ts.addParser({
        id: "ipAddress",
        is: function(s) {
            return /^\d{2,3}[\.]\d{2,3}[\.]\d{2,3}[\.]\d{2,3}$/.test(s);
        },
        format: function(s) {
            var a = s.split("."),
                r = "",
                l = a.length;
            for (var i = 0; i < l; i++) {
                var item = a[i];
                if (item.length == 2) {
                    r += "0" + item;
                } else {
                    r += item;
                }
            }
            return $.tablesorter.formatFloat(r);
        },
        type: "numeric"
    });
    ts.addParser({
        id: "url",
        is: function(s) {
            return /^(https?|ftp|file):\/\/$/.test(s);
        },
        format: function(s) {
            return jQuery.trim(s.replace(new RegExp(/(https?|ftp|file):\/\//), ''));
        },
        type: "text"
    });
    ts.addParser({
        id: "isoDate",
        is: function(s) {
            return /^\d{4}[\/-]\d{1,2}[\/-]\d{1,2}$/.test(s);
        },
        format: function(s) {
            return $.tablesorter.formatFloat((s != "") ? new Date(s.replace(new RegExp(/-/g), "/")).getTime() : "0");
        },
        type: "numeric"
    });
    ts.addParser({
        id: "percent",
        is: function(s) {
            return /\%$/.test($.trim(s));
        },
        format: function(s) {
            return $.tablesorter.formatFloat(s.replace(new RegExp(/%/g), ""));
        },
        type: "numeric"
    });
    ts.addParser({
        id: "usLongDate",
        is: function(s) {
            return s.match(new RegExp(/^[A-Za-z]{3,10}\.? [0-9]{1,2}, ([0-9]{4}|'?[0-9]{2}) (([0-2]?[0-9]:[0-5][0-9])|([0-1]?[0-9]:[0-5][0-9]\s(AM|PM)))$/));
        },
        format: function(s) {
            return $.tablesorter.formatFloat(new Date(s).getTime());
        },
        type: "numeric"
    });
    ts.addParser({
        id: "shortDate",
        is: function(s) {
            return /\d{1,2}[\/\-]\d{1,2}[\/\-]\d{2,4}/.test(s);
        },
        format: function(s, table) {
            var c = table.config;
            s = s.replace(/\-/g, "/");
            if (c.dateFormat == "us") {
                s = s.replace(/(\d{1,2})[\/\-](\d{1,2})[\/\-](\d{4})/, "$3/$1/$2");
            } else if (c.dateFormat == "uk") {
                s = s.replace(/(\d{1,2})[\/\-](\d{1,2})[\/\-](\d{4})/, "$3/$2/$1");
            } else if (c.dateFormat == "dd/mm/yy" || c.dateFormat == "dd-mm-yy") {
                s = s.replace(/(\d{1,2})[\/\-](\d{1,2})[\/\-](\d{2})/, "$1/$2/$3");
            }
            return $.tablesorter.formatFloat(new Date(s).getTime());
        },
        type: "numeric"
    });
    ts.addParser({
        id: "time",
        is: function(s) {
            return /^(([0-2]?[0-9]:[0-5][0-9])|([0-1]?[0-9]:[0-5][0-9]\s(am|pm)))$/.test(s);
        },
        format: function(s) {
            return $.tablesorter.formatFloat(new Date("2000/01/01 " + s).getTime());
        },
        type: "numeric"
    });
    ts.addParser({
        id: "metadata",
        is: function(s) {
            return false;
        },
        format: function(s, table, cell) {
            var c = table.config,
                p = (!c.parserMetadataName) ? 'sortValue' : c.parserMetadataName;
            return $(cell).metadata()[p];
        },
        type: "numeric"
    });
    ts.addWidget({
        id: "zebra",
        format: function(table) {
            if (table.config.debug) {
                var time = new Date();
            }
            var $tr, row = -1,
                odd;
            $("tr:visible", table.tBodies[0]).each(function(i) {
                $tr = $(this);
                if (!$tr.hasClass(table.config.cssChildRow)) row++;
                odd = (row % 2 == 0);
                $tr.removeClass(table.config.widgetZebra.css[odd ? 0 : 1]).addClass(table.config.widgetZebra.css[odd ? 1 : 0])
            });
            if (table.config.debug) {
                $.tablesorter.benchmark("Applying Zebra widget", time);
            }
        }
    });
})(jQuery);

! function(t) {
    if ("object" == typeof exports && "undefined" != typeof module) module.exports = t();
    else if ("function" == typeof define && define.amd) define([], t);
    else {
        var e;
        e = "undefined" != typeof window ? window : "undefined" != typeof global ? global : "undefined" != typeof self ? self : this, e.Chart = t()
    }
}(function() {
    return function t(e, a, i) {
        function n(r, l) {
            if (!a[r]) {
                if (!e[r]) {
                    var s = "function" == typeof require && require;
                    if (!l && s) return s(r, !0);
                    if (o) return o(r, !0);
                    var d = new Error("Cannot find module '" + r + "'");
                    throw d.code = "MODULE_NOT_FOUND", d
                }
                var u = a[r] = {
                    exports: {}
                };
                e[r][0].call(u.exports, function(t) {
                    var a = e[r][1][t];
                    return n(a ? a : t)
                }, u, u.exports, t, e, a, i)
            }
            return a[r].exports
        }
        for (var o = "function" == typeof require && require, r = 0; r < i.length; r++) n(i[r]);
        return n
    }({
        1: [function(t, e, a) {}, {}],
        2: [function(t, e, a) {
            function i(t) {
                if (t) {
                    var e = /^#([a-fA-F0-9]{3})$/,
                        a = /^#([a-fA-F0-9]{6})$/,
                        i = /^rgba?\(\s*([+-]?\d+)\s*,\s*([+-]?\d+)\s*,\s*([+-]?\d+)\s*(?:,\s*([+-]?[\d\.]+)\s*)?\)$/,
                        n = /^rgba?\(\s*([+-]?[\d\.]+)\%\s*,\s*([+-]?[\d\.]+)\%\s*,\s*([+-]?[\d\.]+)\%\s*(?:,\s*([+-]?[\d\.]+)\s*)?\)$/,
                        o = /(\w+)/,
                        r = [0, 0, 0],
                        l = 1,
                        s = t.match(e);
                    if (s) {
                        s = s[1];
                        for (var d = 0; d < r.length; d++) r[d] = parseInt(s[d] + s[d], 16)
                    } else if (s = t.match(a)) {
                        s = s[1];
                        for (var d = 0; d < r.length; d++) r[d] = parseInt(s.slice(2 * d, 2 * d + 2), 16)
                    } else if (s = t.match(i)) {
                        for (var d = 0; d < r.length; d++) r[d] = parseInt(s[d + 1]);
                        l = parseFloat(s[4])
                    } else if (s = t.match(n)) {
                        for (var d = 0; d < r.length; d++) r[d] = Math.round(2.55 * parseFloat(s[d + 1]));
                        l = parseFloat(s[4])
                    } else if (s = t.match(o)) {
                        if ("transparent" == s[1]) return [0, 0, 0, 0];
                        if (r = y[s[1]], !r) return
                    }
                    for (var d = 0; d < r.length; d++) r[d] = v(r[d], 0, 255);
                    return l = l || 0 == l ? v(l, 0, 1) : 1, r[3] = l, r
                }
            }

            function n(t) {
                if (t) {
                    var e = /^hsla?\(\s*([+-]?\d+)(?:deg)?\s*,\s*([+-]?[\d\.]+)%\s*,\s*([+-]?[\d\.]+)%\s*(?:,\s*([+-]?[\d\.]+)\s*)?\)/,
                        a = t.match(e);
                    if (a) {
                        var i = parseFloat(a[4]),
                            n = v(parseInt(a[1]), 0, 360),
                            o = v(parseFloat(a[2]), 0, 100),
                            r = v(parseFloat(a[3]), 0, 100),
                            l = v(isNaN(i) ? 1 : i, 0, 1);
                        return [n, o, r, l]
                    }
                }
            }

            function o(t) {
                if (t) {
                    var e = /^hwb\(\s*([+-]?\d+)(?:deg)?\s*,\s*([+-]?[\d\.]+)%\s*,\s*([+-]?[\d\.]+)%\s*(?:,\s*([+-]?[\d\.]+)\s*)?\)/,
                        a = t.match(e);
                    if (a) {
                        var i = parseFloat(a[4]),
                            n = v(parseInt(a[1]), 0, 360),
                            o = v(parseFloat(a[2]), 0, 100),
                            r = v(parseFloat(a[3]), 0, 100),
                            l = v(isNaN(i) ? 1 : i, 0, 1);
                        return [n, o, r, l]
                    }
                }
            }

            function r(t) {
                var e = i(t);
                return e && e.slice(0, 3)
            }

            function l(t) {
                var e = n(t);
                return e && e.slice(0, 3)
            }

            function s(t) {
                var e = i(t);
                return e ? e[3] : (e = n(t)) ? e[3] : (e = o(t)) ? e[3] : void 0
            }

            function d(t) {
                return "#" + x(t[0]) + x(t[1]) + x(t[2])
            }

            function u(t, e) {
                return 1 > e || t[3] && t[3] < 1 ? c(t, e) : "rgb(" + t[0] + ", " + t[1] + ", " + t[2] + ")"
            }

            function c(t, e) {
                return void 0 === e && (e = void 0 !== t[3] ? t[3] : 1), "rgba(" + t[0] + ", " + t[1] + ", " + t[2] + ", " + e + ")"
            }

            function h(t, e) {
                if (1 > e || t[3] && t[3] < 1) return f(t, e);
                var a = Math.round(t[0] / 255 * 100),
                    i = Math.round(t[1] / 255 * 100),
                    n = Math.round(t[2] / 255 * 100);
                return "rgb(" + a + "%, " + i + "%, " + n + "%)"
            }

            function f(t, e) {
                var a = Math.round(t[0] / 255 * 100),
                    i = Math.round(t[1] / 255 * 100),
                    n = Math.round(t[2] / 255 * 100);
                return "rgba(" + a + "%, " + i + "%, " + n + "%, " + (e || t[3] || 1) + ")"
            }

            function g(t, e) {
                return 1 > e || t[3] && t[3] < 1 ? p(t, e) : "hsl(" + t[0] + ", " + t[1] + "%, " + t[2] + "%)"
            }

            function p(t, e) {
                return void 0 === e && (e = void 0 !== t[3] ? t[3] : 1), "hsla(" + t[0] + ", " + t[1] + "%, " + t[2] + "%, " + e + ")"
            }

            function m(t, e) {
                return void 0 === e && (e = void 0 !== t[3] ? t[3] : 1), "hwb(" + t[0] + ", " + t[1] + "%, " + t[2] + "%" + (void 0 !== e && 1 !== e ? ", " + e : "") + ")"
            }

            function b(t) {
                return k[t.slice(0, 3)]
            }

            function v(t, e, a) {
                return Math.min(Math.max(e, t), a)
            }

            function x(t) {
                var e = t.toString(16).toUpperCase();
                return e.length < 2 ? "0" + e : e
            }
            var y = t(6);
            e.exports = {
                getRgba: i,
                getHsla: n,
                getRgb: r,
                getHsl: l,
                getHwb: o,
                getAlpha: s,
                hexString: d,
                rgbString: u,
                rgbaString: c,
                percentString: h,
                percentaString: f,
                hslString: g,
                hslaString: p,
                hwbString: m,
                keyword: b
            };
            var k = {};
            for (var S in y) k[y[S]] = S
        }, {
            6: 6
        }],
        3: [function(t, e, a) {
            var i = t(5),
                n = t(2),
                o = function(t) {
                    if (t instanceof o) return t;
                    if (!(this instanceof o)) return new o(t);
                    this.values = {
                        rgb: [0, 0, 0],
                        hsl: [0, 0, 0],
                        hsv: [0, 0, 0],
                        hwb: [0, 0, 0],
                        cmyk: [0, 0, 0, 0],
                        alpha: 1
                    };
                    var e;
                    if ("string" == typeof t)
                        if (e = n.getRgba(t)) this.setValues("rgb", e);
                        else if (e = n.getHsla(t)) this.setValues("hsl", e);
                    else {
                        if (!(e = n.getHwb(t))) throw new Error('Unable to parse color from string "' + t + '"');
                        this.setValues("hwb", e)
                    } else if ("object" == typeof t)
                        if (e = t, void 0 !== e.r || void 0 !== e.red) this.setValues("rgb", e);
                        else if (void 0 !== e.l || void 0 !== e.lightness) this.setValues("hsl", e);
                    else if (void 0 !== e.v || void 0 !== e.value) this.setValues("hsv", e);
                    else if (void 0 !== e.w || void 0 !== e.whiteness) this.setValues("hwb", e);
                    else {
                        if (void 0 === e.c && void 0 === e.cyan) throw new Error("Unable to parse color from object " + JSON.stringify(t));
                        this.setValues("cmyk", e)
                    }
                };
            o.prototype = {
                rgb: function() {
                    return this.setSpace("rgb", arguments)
                },
                hsl: function() {
                    return this.setSpace("hsl", arguments)
                },
                hsv: function() {
                    return this.setSpace("hsv", arguments)
                },
                hwb: function() {
                    return this.setSpace("hwb", arguments)
                },
                cmyk: function() {
                    return this.setSpace("cmyk", arguments)
                },
                rgbArray: function() {
                    return this.values.rgb
                },
                hslArray: function() {
                    return this.values.hsl
                },
                hsvArray: function() {
                    return this.values.hsv
                },
                hwbArray: function() {
                    var t = this.values;
                    return 1 !== t.alpha ? t.hwb.concat([t.alpha]) : t.hwb
                },
                cmykArray: function() {
                    return this.values.cmyk
                },
                rgbaArray: function() {
                    var t = this.values;
                    return t.rgb.concat([t.alpha])
                },
                hslaArray: function() {
                    var t = this.values;
                    return t.hsl.concat([t.alpha])
                },
                alpha: function(t) {
                    return void 0 === t ? this.values.alpha : (this.setValues("alpha", t), this)
                },
                red: function(t) {
                    return this.setChannel("rgb", 0, t)
                },
                green: function(t) {
                    return this.setChannel("rgb", 1, t)
                },
                blue: function(t) {
                    return this.setChannel("rgb", 2, t)
                },
                hue: function(t) {
                    return t && (t %= 360, t = 0 > t ? 360 + t : t), this.setChannel("hsl", 0, t)
                },
                saturation: function(t) {
                    return this.setChannel("hsl", 1, t)
                },
                lightness: function(t) {
                    return this.setChannel("hsl", 2, t)
                },
                saturationv: function(t) {
                    return this.setChannel("hsv", 1, t)
                },
                whiteness: function(t) {
                    return this.setChannel("hwb", 1, t)
                },
                blackness: function(t) {
                    return this.setChannel("hwb", 2, t)
                },
                value: function(t) {
                    return this.setChannel("hsv", 2, t)
                },
                cyan: function(t) {
                    return this.setChannel("cmyk", 0, t)
                },
                magenta: function(t) {
                    return this.setChannel("cmyk", 1, t)
                },
                yellow: function(t) {
                    return this.setChannel("cmyk", 2, t)
                },
                black: function(t) {
                    return this.setChannel("cmyk", 3, t)
                },
                hexString: function() {
                    return n.hexString(this.values.rgb)
                },
                rgbString: function() {
                    return n.rgbString(this.values.rgb, this.values.alpha)
                },
                rgbaString: function() {
                    return n.rgbaString(this.values.rgb, this.values.alpha)
                },
                percentString: function() {
                    return n.percentString(this.values.rgb, this.values.alpha)
                },
                hslString: function() {
                    return n.hslString(this.values.hsl, this.values.alpha)
                },
                hslaString: function() {
                    return n.hslaString(this.values.hsl, this.values.alpha)
                },
                hwbString: function() {
                    return n.hwbString(this.values.hwb, this.values.alpha)
                },
                keyword: function() {
                    return n.keyword(this.values.rgb, this.values.alpha)
                },
                rgbNumber: function() {
                    var t = this.values.rgb;
                    return t[0] << 16 | t[1] << 8 | t[2]
                },
                luminosity: function() {
                    for (var t = this.values.rgb, e = [], a = 0; a < t.length; a++) {
                        var i = t[a] / 255;
                        e[a] = .03928 >= i ? i / 12.92 : Math.pow((i + .055) / 1.055, 2.4)
                    }
                    return .2126 * e[0] + .7152 * e[1] + .0722 * e[2]
                },
                contrast: function(t) {
                    var e = this.luminosity(),
                        a = t.luminosity();
                    return e > a ? (e + .05) / (a + .05) : (a + .05) / (e + .05)
                },
                level: function(t) {
                    var e = this.contrast(t);
                    return e >= 7.1 ? "AAA" : e >= 4.5 ? "AA" : ""
                },
                dark: function() {
                    var t = this.values.rgb,
                        e = (299 * t[0] + 587 * t[1] + 114 * t[2]) / 1e3;
                    return 128 > e
                },
                light: function() {
                    return !this.dark()
                },
                negate: function() {
                    for (var t = [], e = 0; 3 > e; e++) t[e] = 255 - this.values.rgb[e];
                    return this.setValues("rgb", t), this
                },
                lighten: function(t) {
                    var e = this.values.hsl;
                    return e[2] += e[2] * t, this.setValues("hsl", e), this
                },
                darken: function(t) {
                    var e = this.values.hsl;
                    return e[2] -= e[2] * t, this.setValues("hsl", e), this
                },
                saturate: function(t) {
                    var e = this.values.hsl;
                    return e[1] += e[1] * t, this.setValues("hsl", e), this
                },
                desaturate: function(t) {
                    var e = this.values.hsl;
                    return e[1] -= e[1] * t, this.setValues("hsl", e), this
                },
                whiten: function(t) {
                    var e = this.values.hwb;
                    return e[1] += e[1] * t, this.setValues("hwb", e), this
                },
                blacken: function(t) {
                    var e = this.values.hwb;
                    return e[2] += e[2] * t, this.setValues("hwb", e), this
                },
                greyscale: function() {
                    var t = this.values.rgb,
                        e = .3 * t[0] + .59 * t[1] + .11 * t[2];
                    return this.setValues("rgb", [e, e, e]), this
                },
                clearer: function(t) {
                    var e = this.values.alpha;
                    return this.setValues("alpha", e - e * t), this
                },
                opaquer: function(t) {
                    var e = this.values.alpha;
                    return this.setValues("alpha", e + e * t), this
                },
                rotate: function(t) {
                    var e = this.values.hsl,
                        a = (e[0] + t) % 360;
                    return e[0] = 0 > a ? 360 + a : a, this.setValues("hsl", e), this
                },
                mix: function(t, e) {
                    var a = this,
                        i = t,
                        n = void 0 === e ? .5 : e,
                        o = 2 * n - 1,
                        r = a.alpha() - i.alpha(),
                        l = ((o * r === -1 ? o : (o + r) / (1 + o * r)) + 1) / 2,
                        s = 1 - l;
                    return this.rgb(l * a.red() + s * i.red(), l * a.green() + s * i.green(), l * a.blue() + s * i.blue()).alpha(a.alpha() * n + i.alpha() * (1 - n))
                },
                toJSON: function() {
                    return this.rgb()
                },
                clone: function() {
                    var t, e, a = new o,
                        i = this.values,
                        n = a.values;
                    for (var r in i) i.hasOwnProperty(r) && (t = i[r], e = {}.toString.call(t), "[object Array]" === e ? n[r] = t.slice(0) : "[object Number]" === e ? n[r] = t : console.error("unexpected color value:", t));
                    return a
                }
            }, o.prototype.spaces = {
                rgb: ["red", "green", "blue"],
                hsl: ["hue", "saturation", "lightness"],
                hsv: ["hue", "saturation", "value"],
                hwb: ["hue", "whiteness", "blackness"],
                cmyk: ["cyan", "magenta", "yellow", "black"]
            }, o.prototype.maxes = {
                rgb: [255, 255, 255],
                hsl: [360, 100, 100],
                hsv: [360, 100, 100],
                hwb: [360, 100, 100],
                cmyk: [100, 100, 100, 100]
            }, o.prototype.getValues = function(t) {
                for (var e = this.values, a = {}, i = 0; i < t.length; i++) a[t.charAt(i)] = e[t][i];
                return 1 !== e.alpha && (a.a = e.alpha), a
            }, o.prototype.setValues = function(t, e) {
                var a, n = this.values,
                    o = this.spaces,
                    r = this.maxes,
                    l = 1;
                if ("alpha" === t) l = e;
                else if (e.length) n[t] = e.slice(0, t.length), l = e[t.length];
                else if (void 0 !== e[t.charAt(0)]) {
                    for (a = 0; a < t.length; a++) n[t][a] = e[t.charAt(a)];
                    l = e.a
                } else if (void 0 !== e[o[t][0]]) {
                    var s = o[t];
                    for (a = 0; a < t.length; a++) n[t][a] = e[s[a]];
                    l = e.alpha
                }
                if (n.alpha = Math.max(0, Math.min(1, void 0 === l ? n.alpha : l)), "alpha" === t) return !1;
                var d;
                for (a = 0; a < t.length; a++) d = Math.max(0, Math.min(r[t][a], n[t][a])), n[t][a] = Math.round(d);
                for (var u in o) u !== t && (n[u] = i[t][u](n[t]));
                return !0
            }, o.prototype.setSpace = function(t, e) {
                var a = e[0];
                return void 0 === a ? this.getValues(t) : ("number" == typeof a && (a = Array.prototype.slice.call(e)), this.setValues(t, a), this)
            }, o.prototype.setChannel = function(t, e, a) {
                var i = this.values[t];
                return void 0 === a ? i[e] : a === i[e] ? this : (i[e] = a, this.setValues(t, i), this)
            }, "undefined" != typeof window && (window.Color = o), e.exports = o
        }, {
            2: 2,
            5: 5
        }],
        4: [function(t, e, a) {
            function i(t) {
                var e, a, i, n = t[0] / 255,
                    o = t[1] / 255,
                    r = t[2] / 255,
                    l = Math.min(n, o, r),
                    s = Math.max(n, o, r),
                    d = s - l;
                return s == l ? e = 0 : n == s ? e = (o - r) / d : o == s ? e = 2 + (r - n) / d : r == s && (e = 4 + (n - o) / d), e = Math.min(60 * e, 360), 0 > e && (e += 360), i = (l + s) / 2, a = s == l ? 0 : .5 >= i ? d / (s + l) : d / (2 - s - l), [e, 100 * a, 100 * i]
            }

            function n(t) {
                var e, a, i, n = t[0],
                    o = t[1],
                    r = t[2],
                    l = Math.min(n, o, r),
                    s = Math.max(n, o, r),
                    d = s - l;
                return a = 0 == s ? 0 : d / s * 1e3 / 10, s == l ? e = 0 : n == s ? e = (o - r) / d : o == s ? e = 2 + (r - n) / d : r == s && (e = 4 + (n - o) / d), e = Math.min(60 * e, 360), 0 > e && (e += 360), i = s / 255 * 1e3 / 10, [e, a, i]
            }

            function o(t) {
                var e = t[0],
                    a = t[1],
                    n = t[2],
                    o = i(t)[0],
                    r = 1 / 255 * Math.min(e, Math.min(a, n)),
                    n = 1 - 1 / 255 * Math.max(e, Math.max(a, n));
                return [o, 100 * r, 100 * n]
            }

            function l(t) {
                var e, a, i, n, o = t[0] / 255,
                    r = t[1] / 255,
                    l = t[2] / 255;
                return n = Math.min(1 - o, 1 - r, 1 - l), e = (1 - o - n) / (1 - n) || 0, a = (1 - r - n) / (1 - n) || 0, i = (1 - l - n) / (1 - n) || 0, [100 * e, 100 * a, 100 * i, 100 * n]
            }

            function s(t) {
                return Z[JSON.stringify(t)]
            }

            function d(t) {
                var e = t[0] / 255,
                    a = t[1] / 255,
                    i = t[2] / 255;
                e = e > .04045 ? Math.pow((e + .055) / 1.055, 2.4) : e / 12.92, a = a > .04045 ? Math.pow((a + .055) / 1.055, 2.4) : a / 12.92, i = i > .04045 ? Math.pow((i + .055) / 1.055, 2.4) : i / 12.92;
                var n = .4124 * e + .3576 * a + .1805 * i,
                    o = .2126 * e + .7152 * a + .0722 * i,
                    r = .0193 * e + .1192 * a + .9505 * i;
                return [100 * n, 100 * o, 100 * r]
            }

            function u(t) {
                var e, a, i, n = d(t),
                    o = n[0],
                    r = n[1],
                    l = n[2];
                return o /= 95.047, r /= 100, l /= 108.883, o = o > .008856 ? Math.pow(o, 1 / 3) : 7.787 * o + 16 / 116, r = r > .008856 ? Math.pow(r, 1 / 3) : 7.787 * r + 16 / 116, l = l > .008856 ? Math.pow(l, 1 / 3) : 7.787 * l + 16 / 116, e = 116 * r - 16, a = 500 * (o - r), i = 200 * (r - l), [e, a, i]
            }

            function c(t) {
                return W(u(t))
            }

            function h(t) {
                var e, a, i, n, o, r = t[0] / 360,
                    l = t[1] / 100,
                    s = t[2] / 100;
                if (0 == l) return o = 255 * s, [o, o, o];
                a = .5 > s ? s * (1 + l) : s + l - s * l, e = 2 * s - a, n = [0, 0, 0];
                for (var d = 0; 3 > d; d++) i = r + 1 / 3 * -(d - 1), 0 > i && i++, i > 1 && i--, o = 1 > 6 * i ? e + 6 * (a - e) * i : 1 > 2 * i ? a : 2 > 3 * i ? e + (a - e) * (2 / 3 - i) * 6 : e, n[d] = 255 * o;
                return n
            }

            function f(t) {
                var e, a, i = t[0],
                    n = t[1] / 100,
                    o = t[2] / 100;
                return 0 === o ? [0, 0, 0] : (o *= 2, n *= 1 >= o ? o : 2 - o, a = (o + n) / 2, e = 2 * n / (o + n), [i, 100 * e, 100 * a])
            }

            function p(t) {
                return o(h(t))
            }

            function m(t) {
                return l(h(t))
            }

            function v(t) {
                return s(h(t))
            }

            function x(t) {
                var e = t[0] / 60,
                    a = t[1] / 100,
                    i = t[2] / 100,
                    n = Math.floor(e) % 6,
                    o = e - Math.floor(e),
                    r = 255 * i * (1 - a),
                    l = 255 * i * (1 - a * o),
                    s = 255 * i * (1 - a * (1 - o)),
                    i = 255 * i;
                switch (n) {
                    case 0:
                        return [i, s, r];
                    case 1:
                        return [l, i, r];
                    case 2:
                        return [r, i, s];
                    case 3:
                        return [r, l, i];
                    case 4:
                        return [s, r, i];
                    case 5:
                        return [i, r, l]
                }
            }

            function y(t) {
                var e, a, i = t[0],
                    n = t[1] / 100,
                    o = t[2] / 100;
                return a = (2 - n) * o, e = n * o, e /= 1 >= a ? a : 2 - a, e = e || 0, a /= 2, [i, 100 * e, 100 * a]
            }

            function k(t) {
                return o(x(t))
            }

            function S(t) {
                return l(x(t))
            }

            function w(t) {
                return s(x(t))
            }

            function C(t) {
                var e, a, i, n, o = t[0] / 360,
                    l = t[1] / 100,
                    s = t[2] / 100,
                    d = l + s;
                switch (d > 1 && (l /= d, s /= d), e = Math.floor(6 * o), a = 1 - s, i = 6 * o - e, 0 != (1 & e) && (i = 1 - i), n = l + i * (a - l), e) {
                    default:
                    case 6:
                    case 0:
                        r = a, g = n, b = l;
                        break;
                    case 1:
                        r = n, g = a, b = l;
                        break;
                    case 2:
                        r = l, g = a, b = n;
                        break;
                    case 3:
                        r = l, g = n, b = a;
                        break;
                    case 4:
                        r = n, g = l, b = a;
                        break;
                    case 5:
                        r = a, g = l, b = n
                }
                return [255 * r, 255 * g, 255 * b]
            }

            function M(t) {
                return i(C(t))
            }

            function D(t) {
                return n(C(t))
            }

            function A(t) {
                return l(C(t))
            }

            function I(t) {
                return s(C(t))
            }

            function T(t) {
                var e, a, i, n = t[0] / 100,
                    o = t[1] / 100,
                    r = t[2] / 100,
                    l = t[3] / 100;
                return e = 1 - Math.min(1, n * (1 - l) + l), a = 1 - Math.min(1, o * (1 - l) + l), i = 1 - Math.min(1, r * (1 - l) + l), [255 * e, 255 * a, 255 * i]
            }

            function F(t) {
                return i(T(t))
            }

            function P(t) {
                return n(T(t))
            }

            function R(t) {
                return o(T(t))
            }

            function _(t) {
                return s(T(t))
            }

            function V(t) {
                var e, a, i, n = t[0] / 100,
                    o = t[1] / 100,
                    r = t[2] / 100;
                return e = 3.2406 * n + -1.5372 * o + r * -.4986, a = n * -.9689 + 1.8758 * o + .0415 * r, i = .0557 * n + o * -.204 + 1.057 * r, e = e > .0031308 ? 1.055 * Math.pow(e, 1 / 2.4) - .055 : e = 12.92 * e, a = a > .0031308 ? 1.055 * Math.pow(a, 1 / 2.4) - .055 : a = 12.92 * a, i = i > .0031308 ? 1.055 * Math.pow(i, 1 / 2.4) - .055 : i = 12.92 * i, e = Math.min(Math.max(0, e), 1), a = Math.min(Math.max(0, a), 1), i = Math.min(Math.max(0, i), 1), [255 * e, 255 * a, 255 * i]
            }

            function L(t) {
                var e, a, i, n = t[0],
                    o = t[1],
                    r = t[2];
                return n /= 95.047, o /= 100, r /= 108.883, n = n > .008856 ? Math.pow(n, 1 / 3) : 7.787 * n + 16 / 116, o = o > .008856 ? Math.pow(o, 1 / 3) : 7.787 * o + 16 / 116, r = r > .008856 ? Math.pow(r, 1 / 3) : 7.787 * r + 16 / 116, e = 116 * o - 16, a = 500 * (n - o), i = 200 * (o - r), [e, a, i]
            }

            function O(t) {
                return W(L(t))
            }

            function B(t) {
                var e, a, i, n, o = t[0],
                    r = t[1],
                    l = t[2];
                return 8 >= o ? (a = 100 * o / 903.3, n = 7.787 * (a / 100) + 16 / 116) : (a = 100 * Math.pow((o + 16) / 116, 3), n = Math.pow(a / 100, 1 / 3)), e = .008856 >= e / 95.047 ? e = 95.047 * (r / 500 + n - 16 / 116) / 7.787 : 95.047 * Math.pow(r / 500 + n, 3), i = .008859 >= i / 108.883 ? i = 108.883 * (n - l / 200 - 16 / 116) / 7.787 : 108.883 * Math.pow(n - l / 200, 3), [e, a, i]
            }

            function W(t) {
                var e, a, i, n = t[0],
                    o = t[1],
                    r = t[2];
                return e = Math.atan2(r, o), a = 360 * e / 2 / Math.PI, 0 > a && (a += 360), i = Math.sqrt(o * o + r * r), [n, i, a]
            }

            function z(t) {
                return V(B(t))
            }

            function N(t) {
                var e, a, i, n = t[0],
                    o = t[1],
                    r = t[2];
                return i = r / 360 * 2 * Math.PI, e = o * Math.cos(i), a = o * Math.sin(i), [n, e, a]
            }

            function H(t) {
                return B(N(t))
            }

            function E(t) {
                return z(N(t))
            }

            function U(t) {
                return Q[t]
            }

            function q(t) {
                return i(U(t))
            }

            function j(t) {
                return n(U(t))
            }

            function Y(t) {
                return o(U(t))
            }

            function J(t) {
                return l(U(t))
            }

            function X(t) {
                return u(U(t))
            }

            function G(t) {
                return d(U(t))
            }
            e.exports = {
                rgb2hsl: i,
                rgb2hsv: n,
                rgb2hwb: o,
                rgb2cmyk: l,
                rgb2keyword: s,
                rgb2xyz: d,
                rgb2lab: u,
                rgb2lch: c,
                hsl2rgb: h,
                hsl2hsv: f,
                hsl2hwb: p,
                hsl2cmyk: m,
                hsl2keyword: v,
                hsv2rgb: x,
                hsv2hsl: y,
                hsv2hwb: k,
                hsv2cmyk: S,
                hsv2keyword: w,
                hwb2rgb: C,
                hwb2hsl: M,
                hwb2hsv: D,
                hwb2cmyk: A,
                hwb2keyword: I,
                cmyk2rgb: T,
                cmyk2hsl: F,
                cmyk2hsv: P,
                cmyk2hwb: R,
                cmyk2keyword: _,
                keyword2rgb: U,
                keyword2hsl: q,
                keyword2hsv: j,
                keyword2hwb: Y,
                keyword2cmyk: J,
                keyword2lab: X,
                keyword2xyz: G,
                xyz2rgb: V,
                xyz2lab: L,
                xyz2lch: O,
                lab2xyz: B,
                lab2rgb: z,
                lab2lch: W,
                lch2lab: N,
                lch2xyz: H,
                lch2rgb: E
            };
            var Q = {
                    aliceblue: [240, 248, 255],
                    antiquewhite: [250, 235, 215],
                    aqua: [0, 255, 255],
                    aquamarine: [127, 255, 212],
                    azure: [240, 255, 255],
                    beige: [245, 245, 220],
                    bisque: [255, 228, 196],
                    black: [0, 0, 0],
                    blanchedalmond: [255, 235, 205],
                    blue: [0, 0, 255],
                    blueviolet: [138, 43, 226],
                    brown: [165, 42, 42],
                    burlywood: [222, 184, 135],
                    cadetblue: [95, 158, 160],
                    chartreuse: [127, 255, 0],
                    chocolate: [210, 105, 30],
                    coral: [255, 127, 80],
                    cornflowerblue: [100, 149, 237],
                    cornsilk: [255, 248, 220],
                    crimson: [220, 20, 60],
                    cyan: [0, 255, 255],
                    darkblue: [0, 0, 139],
                    darkcyan: [0, 139, 139],
                    darkgoldenrod: [184, 134, 11],
                    darkgray: [169, 169, 169],
                    darkgreen: [0, 100, 0],
                    darkgrey: [169, 169, 169],
                    darkkhaki: [189, 183, 107],
                    darkmagenta: [139, 0, 139],
                    darkolivegreen: [85, 107, 47],
                    darkorange: [255, 140, 0],
                    darkorchid: [153, 50, 204],
                    darkred: [139, 0, 0],
                    darksalmon: [233, 150, 122],
                    darkseagreen: [143, 188, 143],
                    darkslateblue: [72, 61, 139],
                    darkslategray: [47, 79, 79],
                    darkslategrey: [47, 79, 79],
                    darkturquoise: [0, 206, 209],
                    darkviolet: [148, 0, 211],
                    deeppink: [255, 20, 147],
                    deepskyblue: [0, 191, 255],
                    dimgray: [105, 105, 105],
                    dimgrey: [105, 105, 105],
                    dodgerblue: [30, 144, 255],
                    firebrick: [178, 34, 34],
                    floralwhite: [255, 250, 240],
                    forestgreen: [34, 139, 34],
                    fuchsia: [255, 0, 255],
                    gainsboro: [220, 220, 220],
                    ghostwhite: [248, 248, 255],
                    gold: [255, 215, 0],
                    goldenrod: [218, 165, 32],
                    gray: [128, 128, 128],
                    green: [0, 128, 0],
                    greenyellow: [173, 255, 47],
                    grey: [128, 128, 128],
                    honeydew: [240, 255, 240],
                    hotpink: [255, 105, 180],
                    indianred: [205, 92, 92],
                    indigo: [75, 0, 130],
                    ivory: [255, 255, 240],
                    khaki: [240, 230, 140],
                    lavender: [230, 230, 250],
                    lavenderblush: [255, 240, 245],
                    lawngreen: [124, 252, 0],
                    lemonchiffon: [255, 250, 205],
                    lightblue: [173, 216, 230],
                    lightcoral: [240, 128, 128],
                    lightcyan: [224, 255, 255],
                    lightgoldenrodyellow: [250, 250, 210],
                    lightgray: [211, 211, 211],
                    lightgreen: [144, 238, 144],
                    lightgrey: [211, 211, 211],
                    lightpink: [255, 182, 193],
                    lightsalmon: [255, 160, 122],
                    lightseagreen: [32, 178, 170],
                    lightskyblue: [135, 206, 250],
                    lightslategray: [119, 136, 153],
                    lightslategrey: [119, 136, 153],
                    lightsteelblue: [176, 196, 222],
                    lightyellow: [255, 255, 224],
                    lime: [0, 255, 0],
                    limegreen: [50, 205, 50],
                    linen: [250, 240, 230],
                    magenta: [255, 0, 255],
                    maroon: [128, 0, 0],
                    mediumaquamarine: [102, 205, 170],
                    mediumblue: [0, 0, 205],
                    mediumorchid: [186, 85, 211],
                    mediumpurple: [147, 112, 219],
                    mediumseagreen: [60, 179, 113],
                    mediumslateblue: [123, 104, 238],
                    mediumspringgreen: [0, 250, 154],
                    mediumturquoise: [72, 209, 204],
                    mediumvioletred: [199, 21, 133],
                    midnightblue: [25, 25, 112],
                    mintcream: [245, 255, 250],
                    mistyrose: [255, 228, 225],
                    moccasin: [255, 228, 181],
                    navajowhite: [255, 222, 173],
                    navy: [0, 0, 128],
                    oldlace: [253, 245, 230],
                    olive: [128, 128, 0],
                    olivedrab: [107, 142, 35],
                    orange: [255, 165, 0],
                    orangered: [255, 69, 0],
                    orchid: [218, 112, 214],
                    palegoldenrod: [238, 232, 170],
                    palegreen: [152, 251, 152],
                    paleturquoise: [175, 238, 238],
                    palevioletred: [219, 112, 147],
                    papayawhip: [255, 239, 213],
                    peachpuff: [255, 218, 185],
                    peru: [205, 133, 63],
                    pink: [255, 192, 203],
                    plum: [221, 160, 221],
                    powderblue: [176, 224, 230],
                    purple: [128, 0, 128],
                    rebeccapurple: [102, 51, 153],
                    red: [255, 0, 0],
                    rosybrown: [188, 143, 143],
                    royalblue: [65, 105, 225],
                    saddlebrown: [139, 69, 19],
                    salmon: [250, 128, 114],
                    sandybrown: [244, 164, 96],
                    seagreen: [46, 139, 87],
                    seashell: [255, 245, 238],
                    sienna: [160, 82, 45],
                    silver: [192, 192, 192],
                    skyblue: [135, 206, 235],
                    slateblue: [106, 90, 205],
                    slategray: [112, 128, 144],
                    slategrey: [112, 128, 144],
                    snow: [255, 250, 250],
                    springgreen: [0, 255, 127],
                    steelblue: [70, 130, 180],
                    tan: [210, 180, 140],
                    teal: [0, 128, 128],
                    thistle: [216, 191, 216],
                    tomato: [255, 99, 71],
                    turquoise: [64, 224, 208],
                    violet: [238, 130, 238],
                    wheat: [245, 222, 179],
                    white: [255, 255, 255],
                    whitesmoke: [245, 245, 245],
                    yellow: [255, 255, 0],
                    yellowgreen: [154, 205, 50]
                },
                Z = {};
            for (var $ in Q) Z[JSON.stringify(Q[$])] = $
        }, {}],
        5: [function(t, e, a) {
            var i = t(4),
                n = function() {
                    return new d
                };
            for (var o in i) {
                n[o + "Raw"] = function(t) {
                    return function(e) {
                        return "number" == typeof e && (e = Array.prototype.slice.call(arguments)), i[t](e)
                    }
                }(o);
                var r = /(\w+)2(\w+)/.exec(o),
                    l = r[1],
                    s = r[2];
                n[l] = n[l] || {}, n[l][s] = n[o] = function(t) {
                    return function(e) {
                        "number" == typeof e && (e = Array.prototype.slice.call(arguments));
                        var a = i[t](e);
                        if ("string" == typeof a || void 0 === a) return a;
                        for (var n = 0; n < a.length; n++) a[n] = Math.round(a[n]);
                        return a
                    }
                }(o)
            }
            var d = function() {
                this.convs = {}
            };
            d.prototype.routeSpace = function(t, e) {
                var a = e[0];
                return void 0 === a ? this.getValues(t) : ("number" == typeof a && (a = Array.prototype.slice.call(e)), this.setValues(t, a))
            }, d.prototype.setValues = function(t, e) {
                return this.space = t, this.convs = {}, this.convs[t] = e, this
            }, d.prototype.getValues = function(t) {
                var e = this.convs[t];
                if (!e) {
                    var a = this.space,
                        i = this.convs[a];
                    e = n[a][t](i), this.convs[t] = e
                }
                return e
            }, ["rgb", "hsl", "hsv", "cmyk", "keyword"].forEach(function(t) {
                d.prototype[t] = function(e) {
                    return this.routeSpace(t, arguments)
                }
            }), e.exports = n
        }, {
            4: 4
        }],
        6: [function(t, e, a) {
            e.exports = {
                aliceblue: [240, 248, 255],
                antiquewhite: [250, 235, 215],
                aqua: [0, 255, 255],
                aquamarine: [127, 255, 212],
                azure: [240, 255, 255],
                beige: [245, 245, 220],
                bisque: [255, 228, 196],
                black: [0, 0, 0],
                blanchedalmond: [255, 235, 205],
                blue: [0, 0, 255],
                blueviolet: [138, 43, 226],
                brown: [165, 42, 42],
                burlywood: [222, 184, 135],
                cadetblue: [95, 158, 160],
                chartreuse: [127, 255, 0],
                chocolate: [210, 105, 30],
                coral: [255, 127, 80],
                cornflowerblue: [100, 149, 237],
                cornsilk: [255, 248, 220],
                crimson: [220, 20, 60],
                cyan: [0, 255, 255],
                darkblue: [0, 0, 139],
                darkcyan: [0, 139, 139],
                darkgoldenrod: [184, 134, 11],
                darkgray: [169, 169, 169],
                darkgreen: [0, 100, 0],
                darkgrey: [169, 169, 169],
                darkkhaki: [189, 183, 107],
                darkmagenta: [139, 0, 139],
                darkolivegreen: [85, 107, 47],
                darkorange: [255, 140, 0],
                darkorchid: [153, 50, 204],
                darkred: [139, 0, 0],
                darksalmon: [233, 150, 122],
                darkseagreen: [143, 188, 143],
                darkslateblue: [72, 61, 139],
                darkslategray: [47, 79, 79],
                darkslategrey: [47, 79, 79],
                darkturquoise: [0, 206, 209],
                darkviolet: [148, 0, 211],
                deeppink: [255, 20, 147],
                deepskyblue: [0, 191, 255],
                dimgray: [105, 105, 105],
                dimgrey: [105, 105, 105],
                dodgerblue: [30, 144, 255],
                firebrick: [178, 34, 34],
                floralwhite: [255, 250, 240],
                forestgreen: [34, 139, 34],
                fuchsia: [255, 0, 255],
                gainsboro: [220, 220, 220],
                ghostwhite: [248, 248, 255],
                gold: [255, 215, 0],
                goldenrod: [218, 165, 32],
                gray: [128, 128, 128],
                green: [0, 128, 0],
                greenyellow: [173, 255, 47],
                grey: [128, 128, 128],
                honeydew: [240, 255, 240],
                hotpink: [255, 105, 180],
                indianred: [205, 92, 92],
                indigo: [75, 0, 130],
                ivory: [255, 255, 240],
                khaki: [240, 230, 140],
                lavender: [230, 230, 250],
                lavenderblush: [255, 240, 245],
                lawngreen: [124, 252, 0],
                lemonchiffon: [255, 250, 205],
                lightblue: [173, 216, 230],
                lightcoral: [240, 128, 128],
                lightcyan: [224, 255, 255],
                lightgoldenrodyellow: [250, 250, 210],
                lightgray: [211, 211, 211],
                lightgreen: [144, 238, 144],
                lightgrey: [211, 211, 211],
                lightpink: [255, 182, 193],
                lightsalmon: [255, 160, 122],
                lightseagreen: [32, 178, 170],
                lightskyblue: [135, 206, 250],
                lightslategray: [119, 136, 153],
                lightslategrey: [119, 136, 153],
                lightsteelblue: [176, 196, 222],
                lightyellow: [255, 255, 224],
                lime: [0, 255, 0],
                limegreen: [50, 205, 50],
                linen: [250, 240, 230],
                magenta: [255, 0, 255],
                maroon: [128, 0, 0],
                mediumaquamarine: [102, 205, 170],
                mediumblue: [0, 0, 205],
                mediumorchid: [186, 85, 211],
                mediumpurple: [147, 112, 219],
                mediumseagreen: [60, 179, 113],
                mediumslateblue: [123, 104, 238],
                mediumspringgreen: [0, 250, 154],
                mediumturquoise: [72, 209, 204],
                mediumvioletred: [199, 21, 133],
                midnightblue: [25, 25, 112],
                mintcream: [245, 255, 250],
                mistyrose: [255, 228, 225],
                moccasin: [255, 228, 181],
                navajowhite: [255, 222, 173],
                navy: [0, 0, 128],
                oldlace: [253, 245, 230],
                olive: [128, 128, 0],
                olivedrab: [107, 142, 35],
                orange: [255, 165, 0],
                orangered: [255, 69, 0],
                orchid: [218, 112, 214],
                palegoldenrod: [238, 232, 170],
                palegreen: [152, 251, 152],
                paleturquoise: [175, 238, 238],
                palevioletred: [219, 112, 147],
                papayawhip: [255, 239, 213],
                peachpuff: [255, 218, 185],
                peru: [205, 133, 63],
                pink: [255, 192, 203],
                plum: [221, 160, 221],
                powderblue: [176, 224, 230],
                purple: [128, 0, 128],
                rebeccapurple: [102, 51, 153],
                red: [255, 0, 0],
                rosybrown: [188, 143, 143],
                royalblue: [65, 105, 225],
                saddlebrown: [139, 69, 19],
                salmon: [250, 128, 114],
                sandybrown: [244, 164, 96],
                seagreen: [46, 139, 87],
                seashell: [255, 245, 238],
                sienna: [160, 82, 45],
                silver: [192, 192, 192],
                skyblue: [135, 206, 235],
                slateblue: [106, 90, 205],
                slategray: [112, 128, 144],
                slategrey: [112, 128, 144],
                snow: [255, 250, 250],
                springgreen: [0, 255, 127],
                steelblue: [70, 130, 180],
                tan: [210, 180, 140],
                teal: [0, 128, 128],
                thistle: [216, 191, 216],
                tomato: [255, 99, 71],
                turquoise: [64, 224, 208],
                violet: [238, 130, 238],
                wheat: [245, 222, 179],
                white: [255, 255, 255],
                whitesmoke: [245, 245, 245],
                yellow: [255, 255, 0],
                yellowgreen: [154, 205, 50]
            }
        }, {}],
        7: [function(t, e, a) {
            var i = t(27)();
            t(26)(i), t(22)(i), t(25)(i), t(21)(i), t(23)(i), t(24)(i), t(28)(i), t(32)(i), t(30)(i), t(31)(i), t(33)(i), t(29)(i), t(34)(i), t(35)(i), t(36)(i), t(37)(i), t(38)(i), t(41)(i), t(39)(i), t(40)(i), t(42)(i), t(43)(i), t(44)(i), t(15)(i), t(16)(i), t(17)(i), t(18)(i), t(19)(i), t(20)(i), t(8)(i), t(9)(i), t(10)(i), t(11)(i), t(12)(i), t(13)(i), t(14)(i), window.Chart = e.exports = i
        }, {
            10: 10,
            11: 11,
            12: 12,
            13: 13,
            14: 14,
            15: 15,
            16: 16,
            17: 17,
            18: 18,
            19: 19,
            20: 20,
            21: 21,
            22: 22,
            23: 23,
            24: 24,
            25: 25,
            26: 26,
            27: 27,
            28: 28,
            29: 29,
            30: 30,
            31: 31,
            32: 32,
            33: 33,
            34: 34,
            35: 35,
            36: 36,
            37: 37,
            38: 38,
            39: 39,
            40: 40,
            41: 41,
            42: 42,
            43: 43,
            44: 44,
            8: 8,
            9: 9
        }],
        8: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                t.Bar = function(e, a) {
                    return a.type = "bar", new t(e, a)
                }
            }
        }, {}],
        9: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                t.Bubble = function(e, a) {
                    return a.type = "bubble", new t(e, a)
                }
            }
        }, {}],
        10: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                t.Doughnut = function(e, a) {
                    return a.type = "doughnut", new t(e, a)
                }
            }
        }, {}],
        11: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                t.Line = function(e, a) {
                    return a.type = "line", new t(e, a)
                }
            }
        }, {}],
        12: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                t.PolarArea = function(e, a) {
                    return a.type = "polarArea", new t(e, a)
                }
            }
        }, {}],
        13: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                t.Radar = function(e, a) {
                    return a.options = t.helpers.configMerge({
                        aspectRatio: 1
                    }, a.options), a.type = "radar", new t(e, a)
                }
            }
        }, {}],
        14: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                var e = {
                    hover: {
                        mode: "single"
                    },
                    scales: {
                        xAxes: [{
                            type: "linear",
                            position: "bottom",
                            id: "x-axis-1"
                        }],
                        yAxes: [{
                            type: "linear",
                            position: "left",
                            id: "y-axis-1"
                        }]
                    },
                    tooltips: {
                        callbacks: {
                            title: function() {
                                return ""
                            },
                            label: function(t) {
                                return "(" + t.xLabel + ", " + t.yLabel + ")"
                            }
                        }
                    }
                };
                t.defaults.scatter = e, t.controllers.scatter = t.controllers.line, t.Scatter = function(e, a) {
                    return a.type = "scatter", new t(e, a)
                }
            }
        }, {}],
        15: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                var e = t.helpers;
                t.defaults.bar = {
                    hover: {
                        mode: "label"
                    },
                    scales: {
                        xAxes: [{
                            type: "category",
                            categoryPercentage: .8,
                            barPercentage: .9,
                            gridLines: {
                                offsetGridLines: !0
                            }
                        }],
                        yAxes: [{
                            type: "linear"
                        }]
                    }
                }, t.controllers.bar = t.DatasetController.extend({
                    dataElementType: t.elements.Rectangle,
                    initialize: function(e, a) {
                        t.DatasetController.prototype.initialize.call(this, e, a), this.getMeta().bar = !0
                    },
                    getBarCount: function() {
                        var t = this,
                            a = 0;
                        return e.each(t.chart.data.datasets, function(e, i) {
                            var n = t.chart.getDatasetMeta(i);
                            n.bar && t.chart.isDatasetVisible(i) && ++a
                        }, t), a
                    },
                    update: function(t) {
                        var a = this;
                        e.each(a.getMeta().data, function(e, i) {
                            a.updateElement(e, i, t)
                        }, a)
                    },
                    updateElement: function(t, a, i) {
                        var n = this,
                            o = n.getMeta(),
                            r = n.getScaleForId(o.xAxisID),
                            l = n.getScaleForId(o.yAxisID),
                            s = l.getBasePixel(),
                            d = n.chart.options.elements.rectangle,
                            u = t.custom || {},
                            c = n.getDataset();
                        e.extend(t, {
                            _xScale: r,
                            _yScale: l,
                            _datasetIndex: n.index,
                            _index: a,
                            _model: {
                                x: n.calculateBarX(a, n.index),
                                y: i ? s : n.calculateBarY(a, n.index),
                                label: n.chart.data.labels[a],
                                datasetLabel: c.label,
                                base: i ? s : n.calculateBarBase(n.index, a),
                                width: n.calculateBarWidth(a),
                                backgroundColor: u.backgroundColor ? u.backgroundColor : e.getValueAtIndexOrDefault(c.backgroundColor, a, d.backgroundColor),
                                borderSkipped: u.borderSkipped ? u.borderSkipped : d.borderSkipped,
                                borderColor: u.borderColor ? u.borderColor : e.getValueAtIndexOrDefault(c.borderColor, a, d.borderColor),
                                borderWidth: u.borderWidth ? u.borderWidth : e.getValueAtIndexOrDefault(c.borderWidth, a, d.borderWidth)
                            }
                        }), t.pivot()
                    },
                    calculateBarBase: function(t, e) {
                        var a = this,
                            i = a.getMeta(),
                            n = a.getScaleForId(i.yAxisID),
                            o = 0;
                        if (n.options.stacked) {
                            for (var r = a.chart, l = r.data.datasets, s = Number(l[t].data[e]), d = 0; t > d; d++) {
                                var u = l[d],
                                    c = r.getDatasetMeta(d);
                                if (c.bar && c.yAxisID === n.id && r.isDatasetVisible(d)) {
                                    var h = Number(u.data[e]);
                                    o += 0 > s ? Math.min(h, 0) : Math.max(h, 0)
                                }
                            }
                            return n.getPixelForValue(o)
                        }
                        return n.getBasePixel()
                    },
                    getRuler: function(t) {
                        var e, a = this,
                            i = a.getMeta(),
                            n = a.getScaleForId(i.xAxisID),
                            o = a.getBarCount();
                        e = "category" === n.options.type ? n.getPixelForTick(t + 1) - n.getPixelForTick(t) : n.width / n.ticks.length;
                        var r = e * n.options.categoryPercentage,
                            l = (e - e * n.options.categoryPercentage) / 2,
                            s = r / o;
                        if (n.ticks.length !== a.chart.data.labels.length) {
                            var d = n.ticks.length / a.chart.data.labels.length;
                            s *= d
                        }
                        var u = s * n.options.barPercentage,
                            c = s - s * n.options.barPercentage;
                        return {
                            datasetCount: o,
                            tickWidth: e,
                            categoryWidth: r,
                            categorySpacing: l,
                            fullBarWidth: s,
                            barWidth: u,
                            barSpacing: c
                        }
                    },
                    calculateBarWidth: function(t) {
                        var e = this.getScaleForId(this.getMeta().xAxisID);
                        if (e.options.barThickness) return e.options.barThickness;
                        var a = this.getRuler(t);
                        return e.options.stacked ? a.categoryWidth : a.barWidth
                    },
                    getBarIndex: function(t) {
                        var e, a, i = 0;
                        for (a = 0; t > a; ++a) e = this.chart.getDatasetMeta(a), e.bar && this.chart.isDatasetVisible(a) && ++i;
                        return i
                    },
                    calculateBarX: function(t, e) {
                        var a = this,
                            i = a.getMeta(),
                            n = a.getScaleForId(i.xAxisID),
                            o = a.getBarIndex(e),
                            r = a.getRuler(t),
                            l = n.getPixelForValue(null, t, e, a.chart.isCombo);
                        return l -= a.chart.isCombo ? r.tickWidth / 2 : 0, n.options.stacked ? l + r.categoryWidth / 2 + r.categorySpacing : l + r.barWidth / 2 + r.categorySpacing + r.barWidth * o + r.barSpacing / 2 + r.barSpacing * o
                    },
                    calculateBarY: function(t, e) {
                        var a = this,
                            i = a.getMeta(),
                            n = a.getScaleForId(i.yAxisID),
                            o = Number(a.getDataset().data[t]);
                        if (n.options.stacked) {
                            for (var r = 0, l = 0, s = 0; e > s; s++) {
                                var d = a.chart.data.datasets[s],
                                    u = a.chart.getDatasetMeta(s);
                                if (u.bar && u.yAxisID === n.id && a.chart.isDatasetVisible(s)) {
                                    var c = Number(d.data[t]);
                                    0 > c ? l += c || 0 : r += c || 0
                                }
                            }
                            return 0 > o ? n.getPixelForValue(l + o) : n.getPixelForValue(r + o)
                        }
                        return n.getPixelForValue(o)
                    },
                    draw: function(t) {
                        var a = this,
                            i = t || 1;
                        e.each(a.getMeta().data, function(t, e) {
                            var n = a.getDataset().data[e];
                            null === n || void 0 === n || isNaN(n) || t.transition(i).draw()
                        }, a)
                    },
                    setHoverStyle: function(t) {
                        var a = this.chart.data.datasets[t._datasetIndex],
                            i = t._index,
                            n = t.custom || {},
                            o = t._model;
                        o.backgroundColor = n.hoverBackgroundColor ? n.hoverBackgroundColor : e.getValueAtIndexOrDefault(a.hoverBackgroundColor, i, e.getHoverColor(o.backgroundColor)), o.borderColor = n.hoverBorderColor ? n.hoverBorderColor : e.getValueAtIndexOrDefault(a.hoverBorderColor, i, e.getHoverColor(o.borderColor)), o.borderWidth = n.hoverBorderWidth ? n.hoverBorderWidth : e.getValueAtIndexOrDefault(a.hoverBorderWidth, i, o.borderWidth)
                    },
                    removeHoverStyle: function(t) {
                        var a = this.chart.data.datasets[t._datasetIndex],
                            i = t._index,
                            n = t.custom || {},
                            o = t._model,
                            r = this.chart.options.elements.rectangle;
                        o.backgroundColor = n.backgroundColor ? n.backgroundColor : e.getValueAtIndexOrDefault(a.backgroundColor, i, r.backgroundColor), o.borderColor = n.borderColor ? n.borderColor : e.getValueAtIndexOrDefault(a.borderColor, i, r.borderColor), o.borderWidth = n.borderWidth ? n.borderWidth : e.getValueAtIndexOrDefault(a.borderWidth, i, r.borderWidth)
                    }
                }), t.defaults.horizontalBar = {
                    hover: {
                        mode: "label"
                    },
                    scales: {
                        xAxes: [{
                            type: "linear",
                            position: "bottom"
                        }],
                        yAxes: [{
                            position: "left",
                            type: "category",
                            categoryPercentage: .8,
                            barPercentage: .9,
                            gridLines: {
                                offsetGridLines: !0
                            }
                        }]
                    },
                    elements: {
                        rectangle: {
                            borderSkipped: "left"
                        }
                    },
                    tooltips: {
                        callbacks: {
                            title: function(t, e) {
                                var a = "";
                                return t.length > 0 && (t[0].yLabel ? a = t[0].yLabel : e.labels.length > 0 && t[0].index < e.labels.length && (a = e.labels[t[0].index])), a
                            },
                            label: function(t, e) {
                                var a = e.datasets[t.datasetIndex].label || "";
                                return a + ": " + t.xLabel
                            }
                        }
                    }
                }, t.controllers.horizontalBar = t.controllers.bar.extend({
                    updateElement: function(t, a, i) {
                        var n = this,
                            o = n.getMeta(),
                            r = n.getScaleForId(o.xAxisID),
                            l = n.getScaleForId(o.yAxisID),
                            s = r.getBasePixel(),
                            d = t.custom || {},
                            u = n.getDataset(),
                            c = n.chart.options.elements.rectangle;
                        e.extend(t, {
                            _xScale: r,
                            _yScale: l,
                            _datasetIndex: n.index,
                            _index: a,
                            _model: {
                                x: i ? s : n.calculateBarX(a, n.index),
                                y: n.calculateBarY(a, n.index),
                                label: n.chart.data.labels[a],
                                datasetLabel: u.label,
                                base: i ? s : n.calculateBarBase(n.index, a),
                                height: n.calculateBarHeight(a),
                                backgroundColor: d.backgroundColor ? d.backgroundColor : e.getValueAtIndexOrDefault(u.backgroundColor, a, c.backgroundColor),
                                borderSkipped: d.borderSkipped ? d.borderSkipped : c.borderSkipped,
                                borderColor: d.borderColor ? d.borderColor : e.getValueAtIndexOrDefault(u.borderColor, a, c.borderColor),
                                borderWidth: d.borderWidth ? d.borderWidth : e.getValueAtIndexOrDefault(u.borderWidth, a, c.borderWidth)
                            },
                            draw: function() {
                                function t(t) {
                                    return s[(u + t) % 4]
                                }
                                var e = this._chart.ctx,
                                    a = this._view,
                                    i = a.height / 2,
                                    n = a.y - i,
                                    o = a.y + i,
                                    r = a.base - (a.base - a.x),
                                    l = a.borderWidth / 2;
                                a.borderWidth && (n += l, o -= l, r += l), e.beginPath(), e.fillStyle = a.backgroundColor, e.strokeStyle = a.borderColor, e.lineWidth = a.borderWidth;
                                var s = [
                                        [a.base, o],
                                        [a.base, n],
                                        [r, n],
                                        [r, o]
                                    ],
                                    d = ["bottom", "left", "top", "right"],
                                    u = d.indexOf(a.borderSkipped, 0); - 1 === u && (u = 0), e.moveTo.apply(e, t(0));
                                for (var c = 1; 4 > c; c++) e.lineTo.apply(e, t(c));
                                e.fill(), a.borderWidth && e.stroke()
                            },
                            inRange: function(t, e) {
                                var a = this._view,
                                    i = !1;
                                return a && (i = a.x < a.base ? e >= a.y - a.height / 2 && e <= a.y + a.height / 2 && t >= a.x && t <= a.base : e >= a.y - a.height / 2 && e <= a.y + a.height / 2 && t >= a.base && t <= a.x), i
                            }
                        }), t.pivot()
                    },
                    calculateBarBase: function(t, e) {
                        var a = this,
                            i = a.getMeta(),
                            n = a.getScaleForId(i.xAxisID),
                            o = 0;
                        if (n.options.stacked) {
                            for (var r = a.chart, l = r.data.datasets, s = Number(l[t].data[e]), d = 0; t > d; d++) {
                                var u = l[d],
                                    c = r.getDatasetMeta(d);
                                if (c.bar && c.xAxisID === n.id && r.isDatasetVisible(d)) {
                                    var h = Number(u.data[e]);
                                    o += 0 > s ? Math.min(h, 0) : Math.max(h, 0)
                                }
                            }
                            return n.getPixelForValue(o)
                        }
                        return n.getBasePixel()
                    },
                    getRuler: function(t) {
                        var e, a = this,
                            i = a.getMeta(),
                            n = a.getScaleForId(i.yAxisID),
                            o = a.getBarCount();
                        e = "category" === n.options.type ? n.getPixelForTick(t + 1) - n.getPixelForTick(t) : n.width / n.ticks.length;
                        var r = e * n.options.categoryPercentage,
                            l = (e - e * n.options.categoryPercentage) / 2,
                            s = r / o;
                        if (n.ticks.length !== a.chart.data.labels.length) {
                            var d = n.ticks.length / a.chart.data.labels.length;
                            s *= d
                        }
                        var u = s * n.options.barPercentage,
                            c = s - s * n.options.barPercentage;
                        return {
                            datasetCount: o,
                            tickHeight: e,
                            categoryHeight: r,
                            categorySpacing: l,
                            fullBarHeight: s,
                            barHeight: u,
                            barSpacing: c
                        }
                    },
                    calculateBarHeight: function(t) {
                        var e = this,
                            a = e.getScaleForId(e.getMeta().yAxisID);
                        if (a.options.barThickness) return a.options.barThickness;
                        var i = e.getRuler(t);
                        return a.options.stacked ? i.categoryHeight : i.barHeight
                    },
                    calculateBarX: function(t, e) {
                        var a = this,
                            i = a.getMeta(),
                            n = a.getScaleForId(i.xAxisID),
                            o = Number(a.getDataset().data[t]);
                        if (n.options.stacked) {
                            for (var r = 0, l = 0, s = 0; e > s; s++) {
                                var d = a.chart.data.datasets[s],
                                    u = a.chart.getDatasetMeta(s);
                                if (u.bar && u.xAxisID === n.id && a.chart.isDatasetVisible(s)) {
                                    var c = Number(d.data[t]);
                                    0 > c ? l += c || 0 : r += c || 0
                                }
                            }
                            return 0 > o ? n.getPixelForValue(l + o) : n.getPixelForValue(r + o)
                        }
                        return n.getPixelForValue(o)
                    },
                    calculateBarY: function(t, e) {
                        var a = this,
                            i = a.getMeta(),
                            n = a.getScaleForId(i.yAxisID),
                            o = a.getBarIndex(e),
                            r = a.getRuler(t),
                            l = n.getPixelForValue(null, t, e, a.chart.isCombo);
                        return l -= a.chart.isCombo ? r.tickHeight / 2 : 0, n.options.stacked ? l + r.categoryHeight / 2 + r.categorySpacing : l + r.barHeight / 2 + r.categorySpacing + r.barHeight * o + r.barSpacing / 2 + r.barSpacing * o
                    }
                })
            }
        }, {}],
        16: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                var e = t.helpers;
                t.defaults.bubble = {
                    hover: {
                        mode: "single"
                    },
                    scales: {
                        xAxes: [{
                            type: "linear",
                            position: "bottom",
                            id: "x-axis-0"
                        }],
                        yAxes: [{
                            type: "linear",
                            position: "left",
                            id: "y-axis-0"
                        }]
                    },
                    tooltips: {
                        callbacks: {
                            title: function() {
                                return ""
                            },
                            label: function(t, e) {
                                var a = e.datasets[t.datasetIndex].label || "",
                                    i = e.datasets[t.datasetIndex].data[t.index];
                                return a + ": (" + i.x + ", " + i.y + ", " + i.r + ")"
                            }
                        }
                    }
                }, t.controllers.bubble = t.DatasetController.extend({
                    dataElementType: t.elements.Point,
                    update: function(t) {
                        var a = this,
                            i = a.getMeta(),
                            n = i.data;
                        e.each(n, function(e, i) {
                            a.updateElement(e, i, t)
                        })
                    },
                    updateElement: function(a, i, n) {
                        var o = this,
                            r = o.getMeta(),
                            l = o.getScaleForId(r.xAxisID),
                            s = o.getScaleForId(r.yAxisID),
                            d = a.custom || {},
                            u = o.getDataset(),
                            c = u.data[i],
                            h = o.chart.options.elements.point,
                            f = o.index;
                        e.extend(a, {
                            _xScale: l,
                            _yScale: s,
                            _datasetIndex: f,
                            _index: i,
                            _model: {
                                x: n ? l.getPixelForDecimal(.5) : l.getPixelForValue("object" == typeof c ? c : NaN, i, f, o.chart.isCombo),
                                y: n ? s.getBasePixel() : s.getPixelForValue(c, i, f),
                                radius: n ? 0 : d.radius ? d.radius : o.getRadius(c),
                                hitRadius: d.hitRadius ? d.hitRadius : e.getValueAtIndexOrDefault(u.hitRadius, i, h.hitRadius)
                            }
                        }), t.DatasetController.prototype.removeHoverStyle.call(o, a, h);
                        var g = a._model;
                        g.skip = d.skip ? d.skip : isNaN(g.x) || isNaN(g.y), a.pivot()
                    },
                    getRadius: function(t) {
                        return t.r || this.chart.options.elements.point.radius
                    },
                    setHoverStyle: function(a) {
                        var i = this;
                        t.DatasetController.prototype.setHoverStyle.call(i, a);
                        var n = i.chart.data.datasets[a._datasetIndex],
                            o = a._index,
                            r = a.custom || {},
                            l = a._model;
                        l.radius = r.hoverRadius ? r.hoverRadius : e.getValueAtIndexOrDefault(n.hoverRadius, o, i.chart.options.elements.point.hoverRadius) + i.getRadius(n.data[o])
                    },
                    removeHoverStyle: function(e) {
                        var a = this;
                        t.DatasetController.prototype.removeHoverStyle.call(a, e, a.chart.options.elements.point);
                        var i = a.chart.data.datasets[e._datasetIndex].data[e._index],
                            n = e.custom || {},
                            o = e._model;
                        o.radius = n.radius ? n.radius : a.getRadius(i)
                    }
                })
            }
        }, {}],
        17: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                var e = t.helpers,
                    a = t.defaults;
                a.doughnut = {
                    animation: {
                        animateRotate: !0,
                        animateScale: !1
                    },
                    aspectRatio: 1,
                    hover: {
                        mode: "single"
                    },
                    legendCallback: function(t) {
                        var e = [];
                        e.push('<ul class="' + t.id + '-legend">');
                        var a = t.data,
                            i = a.datasets,
                            n = a.labels;
                        if (i.length)
                            for (var o = 0; o < i[0].data.length; ++o) e.push('<li><span style="background-color:' + i[0].backgroundColor[o] + '"></span>'), n[o] && e.push(n[o]), e.push("</li>");
                        return e.push("</ul>"), e.join("")
                    },
                    legend: {
                        labels: {
                            generateLabels: function(t) {
                                var a = t.data;
                                return a.labels.length && a.datasets.length ? a.labels.map(function(i, n) {
                                    var o = t.getDatasetMeta(0),
                                        r = a.datasets[0],
                                        l = o.data[n],
                                        s = l && l.custom || {},
                                        d = e.getValueAtIndexOrDefault,
                                        u = t.options.elements.arc,
                                        c = s.backgroundColor ? s.backgroundColor : d(r.backgroundColor, n, u.backgroundColor),
                                        h = s.borderColor ? s.borderColor : d(r.borderColor, n, u.borderColor),
                                        f = s.borderWidth ? s.borderWidth : d(r.borderWidth, n, u.borderWidth);
                                    return {
                                        text: i,
                                        fillStyle: c,
                                        strokeStyle: h,
                                        lineWidth: f,
                                        hidden: isNaN(r.data[n]) || o.data[n].hidden,
                                        index: n
                                    }
                                }) : []
                            }
                        },
                        onClick: function(t, e) {
                            var a, i, n, o = e.index,
                                r = this.chart;
                            for (a = 0, i = (r.data.datasets || []).length; i > a; ++a) n = r.getDatasetMeta(a), n.data[o].hidden = !n.data[o].hidden;
                            r.update()
                        }
                    },
                    cutoutPercentage: 50,
                    rotation: Math.PI * -.5,
                    circumference: 2 * Math.PI,
                    tooltips: {
                        callbacks: {
                            title: function() {
                                return ""
                            },
                            label: function(t, e) {
                                return e.labels[t.index] + ": " + e.datasets[t.datasetIndex].data[t.index]
                            }
                        }
                    }
                }, a.pie = e.clone(a.doughnut), e.extend(a.pie, {
                    cutoutPercentage: 0
                }), t.controllers.doughnut = t.controllers.pie = t.DatasetController.extend({
                    dataElementType: t.elements.Arc,
                    linkScales: e.noop,
                    getRingIndex: function(t) {
                        for (var e = 0, a = 0; t > a; ++a) this.chart.isDatasetVisible(a) && ++e;
                        return e
                    },
                    update: function(t) {
                        var a = this,
                            i = a.chart,
                            n = i.chartArea,
                            o = i.options,
                            r = o.elements.arc,
                            l = n.right - n.left - r.borderWidth,
                            s = n.bottom - n.top - r.borderWidth,
                            d = Math.min(l, s),
                            u = {
                                x: 0,
                                y: 0
                            },
                            c = a.getMeta(),
                            h = o.cutoutPercentage,
                            f = o.circumference;
                        if (f < 2 * Math.PI) {
                            var g = o.rotation % (2 * Math.PI);
                            g += 2 * Math.PI * (g >= Math.PI ? -1 : g < -Math.PI ? 1 : 0);
                            var p = g + f,
                                m = {
                                    x: Math.cos(g),
                                    y: Math.sin(g)
                                },
                                b = {
                                    x: Math.cos(p),
                                    y: Math.sin(p)
                                },
                                v = 0 >= g && p >= 0 || g <= 2 * Math.PI && 2 * Math.PI <= p,
                                x = g <= .5 * Math.PI && .5 * Math.PI <= p || g <= 2.5 * Math.PI && 2.5 * Math.PI <= p,
                                y = g <= -Math.PI && -Math.PI <= p || g <= Math.PI && Math.PI <= p,
                                k = g <= .5 * -Math.PI && .5 * -Math.PI <= p || g <= 1.5 * Math.PI && 1.5 * Math.PI <= p,
                                S = h / 100,
                                w = {
                                    x: y ? -1 : Math.min(m.x * (m.x < 0 ? 1 : S), b.x * (b.x < 0 ? 1 : S)),
                                    y: k ? -1 : Math.min(m.y * (m.y < 0 ? 1 : S), b.y * (b.y < 0 ? 1 : S))
                                },
                                C = {
                                    x: v ? 1 : Math.max(m.x * (m.x > 0 ? 1 : S), b.x * (b.x > 0 ? 1 : S)),
                                    y: x ? 1 : Math.max(m.y * (m.y > 0 ? 1 : S), b.y * (b.y > 0 ? 1 : S))
                                },
                                M = {
                                    width: .5 * (C.x - w.x),
                                    height: .5 * (C.y - w.y)
                                };
                            d = Math.min(l / M.width, s / M.height), u = {
                                x: (C.x + w.x) * -.5,
                                y: (C.y + w.y) * -.5
                            }
                        }
                        i.borderWidth = a.getMaxBorderWidth(c.data), i.outerRadius = Math.max((d - i.borderWidth) / 2, 0), i.innerRadius = Math.max(h ? i.outerRadius / 100 * h : 1, 0), i.radiusLength = (i.outerRadius - i.innerRadius) / i.getVisibleDatasetCount(), i.offsetX = u.x * i.outerRadius, i.offsetY = u.y * i.outerRadius, c.total = a.calculateTotal(), a.outerRadius = i.outerRadius - i.radiusLength * a.getRingIndex(a.index), a.innerRadius = a.outerRadius - i.radiusLength, e.each(c.data, function(e, i) {
                            a.updateElement(e, i, t)
                        })
                    },
                    updateElement: function(t, a, i) {
                        var n = this,
                            o = n.chart,
                            r = o.chartArea,
                            l = o.options,
                            s = l.animation,
                            d = (r.left + r.right) / 2,
                            u = (r.top + r.bottom) / 2,
                            c = l.rotation,
                            h = l.rotation,
                            f = n.getDataset(),
                            g = i && s.animateRotate ? 0 : t.hidden ? 0 : n.calculateCircumference(f.data[a]) * (l.circumference / (2 * Math.PI)),
                            p = i && s.animateScale ? 0 : n.innerRadius,
                            m = i && s.animateScale ? 0 : n.outerRadius,
                            b = e.getValueAtIndexOrDefault;
                        e.extend(t, {
                            _datasetIndex: n.index,
                            _index: a,
                            _model: {
                                x: d + o.offsetX,
                                y: u + o.offsetY,
                                startAngle: c,
                                endAngle: h,
                                circumference: g,
                                outerRadius: m,
                                innerRadius: p,
                                label: b(f.label, a, o.data.labels[a])
                            }
                        });
                        var v = t._model;
                        this.removeHoverStyle(t), i && s.animateRotate || (0 === a ? v.startAngle = l.rotation : v.startAngle = n.getMeta().data[a - 1]._model.endAngle, v.endAngle = v.startAngle + v.circumference), t.pivot()
                    },
                    removeHoverStyle: function(e) {
                        t.DatasetController.prototype.removeHoverStyle.call(this, e, this.chart.options.elements.arc)
                    },
                    calculateTotal: function() {
                        var t, a = this.getDataset(),
                            i = this.getMeta(),
                            n = 0;
                        return e.each(i.data, function(e, i) {
                            t = a.data[i], isNaN(t) || e.hidden || (n += Math.abs(t))
                        }), n
                    },
                    calculateCircumference: function(t) {
                        var e = this.getMeta().total;
                        return e > 0 && !isNaN(t) ? 2 * Math.PI * (t / e) : 0
                    },
                    getMaxBorderWidth: function(t) {
                        for (var e, a, i = 0, n = this.index, o = t.length, r = 0; o > r; r++) e = t[r]._model ? t[r]._model.borderWidth : 0, a = t[r]._chart ? t[r]._chart.config.data.datasets[n].hoverBorderWidth : 0, i = e > i ? e : i, i = a > i ? a : i;
                        return i
                    }
                })
            }
        }, {}],
        18: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                function e(t, e) {
                    return a.getValueOrDefault(t.showLine, e.showLines)
                }
                var a = t.helpers;
                t.defaults.line = {
                    showLines: !0,
                    spanGaps: !1,
                    hover: {
                        mode: "label"
                    },
                    scales: {
                        xAxes: [{
                            type: "category",
                            id: "x-axis-0"
                        }],
                        yAxes: [{
                            type: "linear",
                            id: "y-axis-0"
                        }]
                    }
                }, t.controllers.line = t.DatasetController.extend({
                    datasetElementType: t.elements.Line,
                    dataElementType: t.elements.Point,
                    addElementAndReset: function(a) {
                        var i = this,
                            n = i.chart.options,
                            o = i.getMeta();
                        t.DatasetController.prototype.addElementAndReset.call(i, a), e(i.getDataset(), n) && 0 !== o.dataset._model.tension && i.updateBezierControlPoints()
                    },
                    update: function(t) {
                        var i, n, o, r = this,
                            l = r.getMeta(),
                            s = l.dataset,
                            d = l.data || [],
                            u = r.chart.options,
                            c = u.elements.line,
                            h = r.getScaleForId(l.yAxisID),
                            f = r.getDataset(),
                            g = e(f, u);
                        for (g && (o = s.custom || {}, void 0 !== f.tension && void 0 === f.lineTension && (f.lineTension = f.tension), s._scale = h, s._datasetIndex = r.index, s._children = d, s._model = {
                                spanGaps: f.spanGaps ? f.spanGaps : u.spanGaps,
                                tension: o.tension ? o.tension : a.getValueOrDefault(f.lineTension, c.tension),
                                backgroundColor: o.backgroundColor ? o.backgroundColor : f.backgroundColor || c.backgroundColor,
                                borderWidth: o.borderWidth ? o.borderWidth : f.borderWidth || c.borderWidth,
                                borderColor: o.borderColor ? o.borderColor : f.borderColor || c.borderColor,
                                borderCapStyle: o.borderCapStyle ? o.borderCapStyle : f.borderCapStyle || c.borderCapStyle,
                                borderDash: o.borderDash ? o.borderDash : f.borderDash || c.borderDash,
                                borderDashOffset: o.borderDashOffset ? o.borderDashOffset : f.borderDashOffset || c.borderDashOffset,
                                borderJoinStyle: o.borderJoinStyle ? o.borderJoinStyle : f.borderJoinStyle || c.borderJoinStyle,
                                fill: o.fill ? o.fill : void 0 !== f.fill ? f.fill : c.fill,
                                steppedLine: o.steppedLine ? o.steppedLine : a.getValueOrDefault(f.steppedLine, c.stepped),
                                scaleTop: h.top,
                                scaleBottom: h.bottom,
                                scaleZero: h.getBasePixel()
                            }, s.pivot()), i = 0, n = d.length; n > i; ++i) r.updateElement(d[i], i, t);
                        for (g && 0 !== s._model.tension && r.updateBezierControlPoints(), i = 0, n = d.length; n > i; ++i) d[i].pivot()
                    },
                    getPointBackgroundColor: function(t, e) {
                        var i = this.chart.options.elements.point.backgroundColor,
                            n = this.getDataset(),
                            o = t.custom || {};
                        return o.backgroundColor ? i = o.backgroundColor : n.pointBackgroundColor ? i = a.getValueAtIndexOrDefault(n.pointBackgroundColor, e, i) : n.backgroundColor && (i = n.backgroundColor), i
                    },
                    getPointBorderColor: function(t, e) {
                        var i = this.chart.options.elements.point.borderColor,
                            n = this.getDataset(),
                            o = t.custom || {};
                        return o.borderColor ? i = o.borderColor : n.pointBorderColor ? i = a.getValueAtIndexOrDefault(n.pointBorderColor, e, i) : n.borderColor && (i = n.borderColor), i
                    },
                    getPointBorderWidth: function(t, e) {
                        var i = this.chart.options.elements.point.borderWidth,
                            n = this.getDataset(),
                            o = t.custom || {};
                        return o.borderWidth ? i = o.borderWidth : n.pointBorderWidth ? i = a.getValueAtIndexOrDefault(n.pointBorderWidth, e, i) : n.borderWidth && (i = n.borderWidth), i
                    },
                    updateElement: function(t, e, i) {
                        var n, o, r = this,
                            l = r.getMeta(),
                            s = t.custom || {},
                            d = r.getDataset(),
                            u = r.index,
                            c = d.data[e],
                            h = r.getScaleForId(l.yAxisID),
                            f = r.getScaleForId(l.xAxisID),
                            g = r.chart.options.elements.point;
                        void 0 !== d.radius && void 0 === d.pointRadius && (d.pointRadius = d.radius), void 0 !== d.hitRadius && void 0 === d.pointHitRadius && (d.pointHitRadius = d.hitRadius), n = f.getPixelForValue("object" == typeof c ? c : NaN, e, u, r.chart.isCombo), o = i ? h.getBasePixel() : r.calculatePointY(c, e, u), t._xScale = f, t._yScale = h, t._datasetIndex = u, t._index = e, t._model = {
                            x: n,
                            y: o,
                            skip: s.skip || isNaN(n) || isNaN(o),
                            radius: s.radius || a.getValueAtIndexOrDefault(d.pointRadius, e, g.radius),
                            pointStyle: s.pointStyle || a.getValueAtIndexOrDefault(d.pointStyle, e, g.pointStyle),
                            backgroundColor: r.getPointBackgroundColor(t, e),
                            borderColor: r.getPointBorderColor(t, e),
                            borderWidth: r.getPointBorderWidth(t, e),
                            tension: l.dataset._model ? l.dataset._model.tension : 0,
                            steppedLine: l.dataset._model ? l.dataset._model.steppedLine : !1,
                            hitRadius: s.hitRadius || a.getValueAtIndexOrDefault(d.pointHitRadius, e, g.hitRadius)
                        }
                    },
                    calculatePointY: function(t, e, a) {
                        var i, n, o, r = this,
                            l = r.chart,
                            s = r.getMeta(),
                            d = r.getScaleForId(s.yAxisID),
                            u = 0,
                            c = 0;
                        if (d.options.stacked) {
                            for (i = 0; a > i; i++)
                                if (n = l.data.datasets[i], o = l.getDatasetMeta(i), "line" === o.type && o.yAxisID === d.id && l.isDatasetVisible(i)) {
                                    var h = Number(d.getRightValue(n.data[e]));
                                    0 > h ? c += h || 0 : u += h || 0
                                } var f = Number(d.getRightValue(t));
                            return 0 > f ? d.getPixelForValue(c + f) : d.getPixelForValue(u + f)
                        }
                        return d.getPixelForValue(t)
                    },
                    updateBezierControlPoints: function() {
                        function t(t, e, a) {
                            return c ? Math.max(Math.min(t, a), e) : t
                        }
                        var e, i, n, o, r, l = this,
                            s = l.getMeta(),
                            d = l.chart.chartArea,
                            u = (s.data || []).filter(function(t) {
                                return !t._model.skip
                            }),
                            c = l.chart.options.elements.line.capBezierPoints;
                        for (e = 0, i = u.length; i > e; ++e) n = u[e], o = n._model, r = a.splineCurve(a.previousItem(u, e)._model, o, a.nextItem(u, e)._model, s.dataset._model.tension), o.controlPointPreviousX = t(r.previous.x, d.left, d.right), o.controlPointPreviousY = t(r.previous.y, d.top, d.bottom), o.controlPointNextX = t(r.next.x, d.left, d.right), o.controlPointNextY = t(r.next.y, d.top, d.bottom)
                    },
                    draw: function(t) {
                        var a, i, n = this,
                            o = n.getMeta(),
                            r = o.data || [],
                            l = t || 1;
                        for (a = 0, i = r.length; i > a; ++a) r[a].transition(l);
                        for (e(n.getDataset(), n.chart.options) && o.dataset.transition(l).draw(), a = 0, i = r.length; i > a; ++a) r[a].draw()
                    },
                    setHoverStyle: function(t) {
                        var e = this.chart.data.datasets[t._datasetIndex],
                            i = t._index,
                            n = t.custom || {},
                            o = t._model;
                        o.radius = n.hoverRadius || a.getValueAtIndexOrDefault(e.pointHoverRadius, i, this.chart.options.elements.point.hoverRadius), o.backgroundColor = n.hoverBackgroundColor || a.getValueAtIndexOrDefault(e.pointHoverBackgroundColor, i, a.getHoverColor(o.backgroundColor)), o.borderColor = n.hoverBorderColor || a.getValueAtIndexOrDefault(e.pointHoverBorderColor, i, a.getHoverColor(o.borderColor)), o.borderWidth = n.hoverBorderWidth || a.getValueAtIndexOrDefault(e.pointHoverBorderWidth, i, o.borderWidth)
                    },
                    removeHoverStyle: function(t) {
                        var e = this,
                            i = e.chart.data.datasets[t._datasetIndex],
                            n = t._index,
                            o = t.custom || {},
                            r = t._model;
                        void 0 !== i.radius && void 0 === i.pointRadius && (i.pointRadius = i.radius), r.radius = o.radius || a.getValueAtIndexOrDefault(i.pointRadius, n, e.chart.options.elements.point.radius), r.backgroundColor = e.getPointBackgroundColor(t, n), r.borderColor = e.getPointBorderColor(t, n), r.borderWidth = e.getPointBorderWidth(t, n)
                    }
                })
            }
        }, {}],
        19: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                var e = t.helpers;
                t.defaults.polarArea = {
                    scale: {
                        type: "radialLinear",
                        lineArc: !0,
                        ticks: {
                            beginAtZero: !0
                        }
                    },
                    animation: {
                        animateRotate: !0,
                        animateScale: !0
                    },
                    startAngle: -.5 * Math.PI,
                    aspectRatio: 1,
                    legendCallback: function(t) {
                        var e = [];
                        e.push('<ul class="' + t.id + '-legend">');
                        var a = t.data,
                            i = a.datasets,
                            n = a.labels;
                        if (i.length)
                            for (var o = 0; o < i[0].data.length; ++o) e.push('<li><span style="background-color:' + i[0].backgroundColor[o] + '">'), n[o] && e.push(n[o]), e.push("</span></li>");
                        return e.push("</ul>"), e.join("")
                    },
                    legend: {
                        labels: {
                            generateLabels: function(t) {
                                var a = t.data;
                                return a.labels.length && a.datasets.length ? a.labels.map(function(i, n) {
                                    var o = t.getDatasetMeta(0),
                                        r = a.datasets[0],
                                        l = o.data[n],
                                        s = l.custom || {},
                                        d = e.getValueAtIndexOrDefault,
                                        u = t.options.elements.arc,
                                        c = s.backgroundColor ? s.backgroundColor : d(r.backgroundColor, n, u.backgroundColor),
                                        h = s.borderColor ? s.borderColor : d(r.borderColor, n, u.borderColor),
                                        f = s.borderWidth ? s.borderWidth : d(r.borderWidth, n, u.borderWidth);
                                    return {
                                        text: i,
                                        fillStyle: c,
                                        strokeStyle: h,
                                        lineWidth: f,
                                        hidden: isNaN(r.data[n]) || o.data[n].hidden,
                                        index: n
                                    }
                                }) : []
                            }
                        },
                        onClick: function(t, e) {
                            var a, i, n, o = e.index,
                                r = this.chart;
                            for (a = 0, i = (r.data.datasets || []).length; i > a; ++a) n = r.getDatasetMeta(a), n.data[o].hidden = !n.data[o].hidden;
                            r.update()
                        }
                    },
                    tooltips: {
                        callbacks: {
                            title: function() {
                                return ""
                            },
                            label: function(t, e) {
                                return e.labels[t.index] + ": " + t.yLabel
                            }
                        }
                    }
                }, t.controllers.polarArea = t.DatasetController.extend({
                    dataElementType: t.elements.Arc,
                    linkScales: e.noop,
                    update: function(t) {
                        var a = this,
                            i = a.chart,
                            n = i.chartArea,
                            o = a.getMeta(),
                            r = i.options,
                            l = r.elements.arc,
                            s = Math.min(n.right - n.left, n.bottom - n.top);
                        i.outerRadius = Math.max((s - l.borderWidth / 2) / 2, 0), i.innerRadius = Math.max(r.cutoutPercentage ? i.outerRadius / 100 * r.cutoutPercentage : 1, 0), i.radiusLength = (i.outerRadius - i.innerRadius) / i.getVisibleDatasetCount(), a.outerRadius = i.outerRadius - i.radiusLength * a.index, a.innerRadius = a.outerRadius - i.radiusLength, o.count = a.countVisibleElements(), e.each(o.data, function(e, i) {
                            a.updateElement(e, i, t)
                        })
                    },
                    updateElement: function(t, a, i) {
                        for (var n = this, o = n.chart, r = n.getDataset(), l = o.options, s = l.animation, d = o.scale, u = e.getValueAtIndexOrDefault, c = o.data.labels, h = n.calculateCircumference(r.data[a]), f = d.xCenter, g = d.yCenter, p = 0, m = n.getMeta(), b = 0; a > b; ++b) isNaN(r.data[b]) || m.data[b].hidden || ++p;
                        var v = l.startAngle,
                            x = t.hidden ? 0 : d.getDistanceFromCenterForValue(r.data[a]),
                            y = v + h * p,
                            k = y + (t.hidden ? 0 : h),
                            S = s.animateScale ? 0 : d.getDistanceFromCenterForValue(r.data[a]);
                        e.extend(t, {
                            _datasetIndex: n.index,
                            _index: a,
                            _scale: d,
                            _model: {
                                x: f,
                                y: g,
                                innerRadius: 0,
                                outerRadius: i ? S : x,
                                startAngle: i && s.animateRotate ? v : y,
                                endAngle: i && s.animateRotate ? v : k,
                                label: u(c, a, c[a])
                            }
                        }), n.removeHoverStyle(t), t.pivot()
                    },
                    removeHoverStyle: function(e) {
                        t.DatasetController.prototype.removeHoverStyle.call(this, e, this.chart.options.elements.arc)
                    },
                    countVisibleElements: function() {
                        var t = this.getDataset(),
                            a = this.getMeta(),
                            i = 0;
                        return e.each(a.data, function(e, a) {
                            isNaN(t.data[a]) || e.hidden || i++
                        }), i
                    },
                    calculateCircumference: function(t) {
                        var e = this.getMeta().count;
                        return e > 0 && !isNaN(t) ? 2 * Math.PI / e : 0
                    }
                })
            }
        }, {}],
        20: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                var e = t.helpers;
                t.defaults.radar = {
                    scale: {
                        type: "radialLinear"
                    },
                    elements: {
                        line: {
                            tension: 0
                        }
                    }
                }, t.controllers.radar = t.DatasetController.extend({
                    datasetElementType: t.elements.Line,
                    dataElementType: t.elements.Point,
                    linkScales: e.noop,
                    addElementAndReset: function(e) {
                        t.DatasetController.prototype.addElementAndReset.call(this, e), this.updateBezierControlPoints()
                    },
                    update: function(t) {
                        var a = this,
                            i = a.getMeta(),
                            n = i.dataset,
                            o = i.data,
                            r = n.custom || {},
                            l = a.getDataset(),
                            s = a.chart.options.elements.line,
                            d = a.chart.scale;
                        void 0 !== l.tension && void 0 === l.lineTension && (l.lineTension = l.tension), e.extend(i.dataset, {
                            _datasetIndex: a.index,
                            _children: o,
                            _loop: !0,
                            _model: {
                                tension: r.tension ? r.tension : e.getValueOrDefault(l.lineTension, s.tension),
                                backgroundColor: r.backgroundColor ? r.backgroundColor : l.backgroundColor || s.backgroundColor,
                                borderWidth: r.borderWidth ? r.borderWidth : l.borderWidth || s.borderWidth,
                                borderColor: r.borderColor ? r.borderColor : l.borderColor || s.borderColor,
                                fill: r.fill ? r.fill : void 0 !== l.fill ? l.fill : s.fill,
                                borderCapStyle: r.borderCapStyle ? r.borderCapStyle : l.borderCapStyle || s.borderCapStyle,
                                borderDash: r.borderDash ? r.borderDash : l.borderDash || s.borderDash,
                                borderDashOffset: r.borderDashOffset ? r.borderDashOffset : l.borderDashOffset || s.borderDashOffset,
                                borderJoinStyle: r.borderJoinStyle ? r.borderJoinStyle : l.borderJoinStyle || s.borderJoinStyle,
                                scaleTop: d.top,
                                scaleBottom: d.bottom,
                                scaleZero: d.getBasePosition()
                            }
                        }), i.dataset.pivot(), e.each(o, function(e, i) {
                            a.updateElement(e, i, t)
                        }, a), a.updateBezierControlPoints()
                    },
                    updateElement: function(t, a, i) {
                        var n = this,
                            o = t.custom || {},
                            r = n.getDataset(),
                            l = n.chart.scale,
                            s = n.chart.options.elements.point,
                            d = l.getPointPositionForValue(a, r.data[a]);
                        e.extend(t, {
                            _datasetIndex: n.index,
                            _index: a,
                            _scale: l,
                            _model: {
                                x: i ? l.xCenter : d.x,
                                y: i ? l.yCenter : d.y,
                                tension: o.tension ? o.tension : e.getValueOrDefault(r.tension, n.chart.options.elements.line.tension),
                                radius: o.radius ? o.radius : e.getValueAtIndexOrDefault(r.pointRadius, a, s.radius),
                                backgroundColor: o.backgroundColor ? o.backgroundColor : e.getValueAtIndexOrDefault(r.pointBackgroundColor, a, s.backgroundColor),
                                borderColor: o.borderColor ? o.borderColor : e.getValueAtIndexOrDefault(r.pointBorderColor, a, s.borderColor),
                                borderWidth: o.borderWidth ? o.borderWidth : e.getValueAtIndexOrDefault(r.pointBorderWidth, a, s.borderWidth),
                                pointStyle: o.pointStyle ? o.pointStyle : e.getValueAtIndexOrDefault(r.pointStyle, a, s.pointStyle),
                                hitRadius: o.hitRadius ? o.hitRadius : e.getValueAtIndexOrDefault(r.hitRadius, a, s.hitRadius)
                            }
                        }), t._model.skip = o.skip ? o.skip : isNaN(t._model.x) || isNaN(t._model.y)
                    },
                    updateBezierControlPoints: function() {
                        var t = this.chart.chartArea,
                            a = this.getMeta();
                        e.each(a.data, function(i, n) {
                            var o = i._model,
                                r = e.splineCurve(e.previousItem(a.data, n, !0)._model, o, e.nextItem(a.data, n, !0)._model, o.tension);
                            o.controlPointPreviousX = Math.max(Math.min(r.previous.x, t.right), t.left), o.controlPointPreviousY = Math.max(Math.min(r.previous.y, t.bottom), t.top), o.controlPointNextX = Math.max(Math.min(r.next.x, t.right), t.left), o.controlPointNextY = Math.max(Math.min(r.next.y, t.bottom), t.top), i.pivot()
                        })
                    },
                    draw: function(t) {
                        var a = this.getMeta(),
                            i = t || 1;
                        e.each(a.data, function(t) {
                            t.transition(i)
                        }), a.dataset.transition(i).draw(), e.each(a.data, function(t) {
                            t.draw()
                        })
                    },
                    setHoverStyle: function(t) {
                        var a = this.chart.data.datasets[t._datasetIndex],
                            i = t.custom || {},
                            n = t._index,
                            o = t._model;
                        o.radius = i.hoverRadius ? i.hoverRadius : e.getValueAtIndexOrDefault(a.pointHoverRadius, n, this.chart.options.elements.point.hoverRadius), o.backgroundColor = i.hoverBackgroundColor ? i.hoverBackgroundColor : e.getValueAtIndexOrDefault(a.pointHoverBackgroundColor, n, e.getHoverColor(o.backgroundColor)), o.borderColor = i.hoverBorderColor ? i.hoverBorderColor : e.getValueAtIndexOrDefault(a.pointHoverBorderColor, n, e.getHoverColor(o.borderColor)), o.borderWidth = i.hoverBorderWidth ? i.hoverBorderWidth : e.getValueAtIndexOrDefault(a.pointHoverBorderWidth, n, o.borderWidth)
                    },
                    removeHoverStyle: function(t) {
                        var a = this.chart.data.datasets[t._datasetIndex],
                            i = t.custom || {},
                            n = t._index,
                            o = t._model,
                            r = this.chart.options.elements.point;
                        o.radius = i.radius ? i.radius : e.getValueAtIndexOrDefault(a.radius, n, r.radius), o.backgroundColor = i.backgroundColor ? i.backgroundColor : e.getValueAtIndexOrDefault(a.pointBackgroundColor, n, r.backgroundColor), o.borderColor = i.borderColor ? i.borderColor : e.getValueAtIndexOrDefault(a.pointBorderColor, n, r.borderColor), o.borderWidth = i.borderWidth ? i.borderWidth : e.getValueAtIndexOrDefault(a.pointBorderWidth, n, r.borderWidth)
                    }
                })
            }
        }, {}],
        21: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                var e = t.helpers;
                t.defaults.global.animation = {
                    duration: 1e3,
                    easing: "easeOutQuart",
                    onProgress: e.noop,
                    onComplete: e.noop
                }, t.Animation = t.Element.extend({
                    currentStep: null,
                    numSteps: 60,
                    easing: "",
                    render: null,
                    onAnimationProgress: null,
                    onAnimationComplete: null
                }), t.animationService = {
                    frameDuration: 17,
                    animations: [],
                    dropFrames: 0,
                    request: null,
                    addAnimation: function(t, e, a, i) {
                        var n = this;
                        i || (t.animating = !0);
                        for (var o = 0; o < n.animations.length; ++o)
                            if (n.animations[o].chartInstance === t) return void(n.animations[o].animationObject = e);
                        n.animations.push({
                            chartInstance: t,
                            animationObject: e
                        }), 1 === n.animations.length && n.requestAnimationFrame()
                    },
                    cancelAnimation: function(t) {
                        var a = e.findIndex(this.animations, function(e) {
                            return e.chartInstance === t
                        }); - 1 !== a && (this.animations.splice(a, 1), t.animating = !1)
                    },
                    requestAnimationFrame: function() {
                        var t = this;
                        null === t.request && (t.request = e.requestAnimFrame.call(window, function() {
                            t.request = null, t.startDigest()
                        }))
                    },
                    startDigest: function() {
                        var t = this,
                            e = Date.now(),
                            a = 0;
                        t.dropFrames > 1 && (a = Math.floor(t.dropFrames), t.dropFrames = t.dropFrames % 1);
                        for (var i = 0; i < t.animations.length;) null === t.animations[i].animationObject.currentStep && (t.animations[i].animationObject.currentStep = 0), t.animations[i].animationObject.currentStep += 1 + a, t.animations[i].animationObject.currentStep > t.animations[i].animationObject.numSteps && (t.animations[i].animationObject.currentStep = t.animations[i].animationObject.numSteps), t.animations[i].animationObject.render(t.animations[i].chartInstance, t.animations[i].animationObject), t.animations[i].animationObject.onAnimationProgress && t.animations[i].animationObject.onAnimationProgress.call && t.animations[i].animationObject.onAnimationProgress.call(t.animations[i].chartInstance, t.animations[i]), t.animations[i].animationObject.currentStep === t.animations[i].animationObject.numSteps ? (t.animations[i].animationObject.onAnimationComplete && t.animations[i].animationObject.onAnimationComplete.call && t.animations[i].animationObject.onAnimationComplete.call(t.animations[i].chartInstance, t.animations[i]), t.animations[i].chartInstance.animating = !1, t.animations.splice(i, 1)) : ++i;
                        var n = Date.now(),
                            o = (n - e) / t.frameDuration;
                        t.dropFrames += o, t.animations.length > 0 && t.requestAnimationFrame()
                    }
                }
            }
        }, {}],
        22: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                var e = t.canvasHelpers = {};
                e.drawPoint = function(t, e, a, i, n) {
                    var o, r, l, s, d, u;
                    if ("object" == typeof e && (o = e.toString(), "[object HTMLImageElement]" === o || "[object HTMLCanvasElement]" === o)) return void t.drawImage(e, i - e.width / 2, n - e.height / 2);
                    if (!(isNaN(a) || 0 >= a)) {
                        switch (e) {
                            default:
                                t.beginPath(), t.arc(i, n, a, 0, 2 * Math.PI), t.closePath(), t.fill();
                                break;
                            case "triangle":
                                t.beginPath(), r = 3 * a / Math.sqrt(3), d = r * Math.sqrt(3) / 2, t.moveTo(i - r / 2, n + d / 3), t.lineTo(i + r / 2, n + d / 3), t.lineTo(i, n - 2 * d / 3), t.closePath(), t.fill();
                                break;
                            case "rect":
                                u = 1 / Math.SQRT2 * a, t.beginPath(), t.fillRect(i - u, n - u, 2 * u, 2 * u), t.strokeRect(i - u, n - u, 2 * u, 2 * u);
                                break;
                            case "rectRot":
                                u = 1 / Math.SQRT2 * a, t.beginPath(), t.moveTo(i - u, n), t.lineTo(i, n + u), t.lineTo(i + u, n), t.lineTo(i, n - u), t.closePath(), t.fill();
                                break;
                            case "cross":
                                t.beginPath(), t.moveTo(i, n + a), t.lineTo(i, n - a), t.moveTo(i - a, n), t.lineTo(i + a, n), t.closePath();
                                break;
                            case "crossRot":
                                t.beginPath(), l = Math.cos(Math.PI / 4) * a, s = Math.sin(Math.PI / 4) * a, t.moveTo(i - l, n - s), t.lineTo(i + l, n + s), t.moveTo(i - l, n + s), t.lineTo(i + l, n - s), t.closePath();
                                break;
                            case "star":
                                t.beginPath(), t.moveTo(i, n + a), t.lineTo(i, n - a), t.moveTo(i - a, n), t.lineTo(i + a, n), l = Math.cos(Math.PI / 4) * a, s = Math.sin(Math.PI / 4) * a, t.moveTo(i - l, n - s), t.lineTo(i + l, n + s), t.moveTo(i - l, n + s), t.lineTo(i + l, n - s), t.closePath();
                                break;
                            case "line":
                                t.beginPath(), t.moveTo(i - a, n), t.lineTo(i + a, n), t.closePath();
                                break;
                            case "dash":
                                t.beginPath(), t.moveTo(i, n), t.lineTo(i + a, n), t.closePath()
                        }
                        t.stroke()
                    }
                }
            }
        }, {}],
        23: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                var e = t.helpers;
                t.types = {}, t.instances = {}, t.controllers = {}, t.Controller = function(a) {
                    return this.chart = a, this.config = a.config, this.options = this.config.options = e.configMerge(t.defaults.global, t.defaults[this.config.type], this.config.options || {}), this.id = e.uid(), Object.defineProperty(this, "data", {
                        get: function() {
                            return this.config.data
                        }
                    }), t.instances[this.id] = this, this.options.responsive && this.resize(!0), this.initialize(), this
                }, e.extend(t.Controller.prototype, {
                    initialize: function() {
                        var e = this;
                        return t.plugins.notify("beforeInit", [e]), e.bindEvents(), e.ensureScalesHaveIDs(), e.buildOrUpdateControllers(), e.buildScales(), e.updateLayout(), e.resetElements(), e.initToolTip(), e.update(), t.plugins.notify("afterInit", [e]), e
                    },
                    clear: function() {
                        return e.clear(this.chart), this
                    },
                    stop: function() {
                        return t.animationService.cancelAnimation(this), this
                    },
                    resize: function(a) {
                        var i = this,
                            n = i.chart,
                            o = n.canvas,
                            r = e.getMaximumWidth(o),
                            l = n.aspectRatio,
                            s = i.options.maintainAspectRatio && isNaN(l) === !1 && isFinite(l) && 0 !== l ? r / l : e.getMaximumHeight(o),
                            d = n.width !== r || n.height !== s;
                        if (!d) return i;
                        o.width = n.width = r, o.height = n.height = s, e.retinaScale(n);
                        var u = {
                            width: r,
                            height: s
                        };
                        return t.plugins.notify("resize", [i, u]), i.options.onResize && i.options.onResize(i, u), a || (i.stop(), i.update(i.options.responsiveAnimationDuration)), i
                    },
                    ensureScalesHaveIDs: function() {
                        var t = this.options,
                            a = t.scales || {},
                            i = t.scale;
                        e.each(a.xAxes, function(t, e) {
                            t.id = t.id || "x-axis-" + e
                        }), e.each(a.yAxes, function(t, e) {
                            t.id = t.id || "y-axis-" + e
                        }), i && (i.id = i.id || "scale")
                    },
                    buildScales: function() {
                        var a = this,
                            i = a.options,
                            n = a.scales = {},
                            o = [];
                        i.scales && (o = o.concat((i.scales.xAxes || []).map(function(t) {
                            return {
                                options: t,
                                dtype: "category"
                            }
                        }), (i.scales.yAxes || []).map(function(t) {
                            return {
                                options: t,
                                dtype: "linear"
                            }
                        }))), i.scale && o.push({
                            options: i.scale,
                            dtype: "radialLinear",
                            isDefault: !0
                        }), e.each(o, function(i) {
                            var o = i.options,
                                r = e.getValueOrDefault(o.type, i.dtype),
                                l = t.scaleService.getScaleConstructor(r);
                            if (l) {
                                var s = new l({
                                    id: o.id,
                                    options: o,
                                    ctx: a.chart.ctx,
                                    chart: a
                                });
                                n[s.id] = s, i.isDefault && (a.scale = s)
                            }
                        }), t.scaleService.addScalesToLayout(this)
                    },
                    updateLayout: function() {
                        t.layoutService.update(this, this.chart.width, this.chart.height)
                    },
                    buildOrUpdateControllers: function() {
                        var a = this,
                            i = [],
                            n = [];
                        if (e.each(a.data.datasets, function(e, o) {
                                var r = a.getDatasetMeta(o);
                                r.type || (r.type = e.type || a.config.type), i.push(r.type), r.controller ? r.controller.updateIndex(o) : (r.controller = new t.controllers[r.type](a, o), n.push(r.controller))
                            }, a), i.length > 1)
                            for (var o = 1; o < i.length; o++)
                                if (i[o] !== i[o - 1]) {
                                    a.isCombo = !0;
                                    break
                                } return n
                    },
                    resetElements: function() {
                        var t = this;
                        e.each(t.data.datasets, function(e, a) {
                            t.getDatasetMeta(a).controller.reset()
                        }, t)
                    },
                    update: function(a, i) {
                        var n = this;
                        t.plugins.notify("beforeUpdate", [n]), n.tooltip._data = n.data;
                        var o = n.buildOrUpdateControllers();
                        e.each(n.data.datasets, function(t, e) {
                            n.getDatasetMeta(e).controller.buildOrUpdateElements()
                        }, n), t.layoutService.update(n, n.chart.width, n.chart.height), t.plugins.notify("afterScaleUpdate", [n]), e.each(o, function(t) {
                            t.reset()
                        }), n.updateDatasets(), t.plugins.notify("afterUpdate", [n]), n.render(a, i)
                    },
                    updateDatasets: function() {
                        var e, a, i = this;
                        if (t.plugins.notify("beforeDatasetsUpdate", [i])) {
                            for (e = 0, a = i.data.datasets.length; a > e; ++e) i.getDatasetMeta(e).controller.update();
                            t.plugins.notify("afterDatasetsUpdate", [i])
                        }
                    },
                    render: function(a, i) {
                        var n = this;
                        t.plugins.notify("beforeRender", [n]);
                        var o = n.options.animation;
                        if (o && ("undefined" != typeof a && 0 !== a || "undefined" == typeof a && 0 !== o.duration)) {
                            var r = new t.Animation;
                            r.numSteps = (a || o.duration) / 16.66, r.easing = o.easing, r.render = function(t, a) {
                                var i = e.easingEffects[a.easing],
                                    n = a.currentStep / a.numSteps,
                                    o = i(n);
                                t.draw(o, n, a.currentStep)
                            }, r.onAnimationProgress = o.onProgress, r.onAnimationComplete = o.onComplete, t.animationService.addAnimation(n, r, a, i)
                        } else n.draw(), o && o.onComplete && o.onComplete.call && o.onComplete.call(n);
                        return n
                    },
                    draw: function(a) {
                        var i = this,
                            n = a || 1;
                        i.clear(), t.plugins.notify("beforeDraw", [i, n]), e.each(i.boxes, function(t) {
                            t.draw(i.chartArea)
                        }, i), i.scale && i.scale.draw(), t.plugins.notify("beforeDatasetsDraw", [i, n]), e.each(i.data.datasets, function(t, e) {
                            i.isDatasetVisible(e) && i.getDatasetMeta(e).controller.draw(a)
                        }, i, !0), t.plugins.notify("afterDatasetsDraw", [i, n]), i.tooltip.transition(n).draw(), t.plugins.notify("afterDraw", [i, n])
                    },
                    getElementAtEvent: function(t) {
                        var a = this,
                            i = e.getRelativePosition(t, a.chart),
                            n = [];
                        return e.each(a.data.datasets, function(t, o) {
                            if (a.isDatasetVisible(o)) {
                                var r = a.getDatasetMeta(o);
                                e.each(r.data, function(t) {
                                    return t.inRange(i.x, i.y) ? (n.push(t), n) : void 0
                                })
                            }
                        }), n.slice(0, 1)
                    },
                    getElementsAtEvent: function(t) {
                        var a = this,
                            i = e.getRelativePosition(t, a.chart),
                            n = [],
                            o = function() {
                                if (a.data.datasets)
                                    for (var t = 0; t < a.data.datasets.length; t++) {
                                        var e = a.getDatasetMeta(t);
                                        if (a.isDatasetVisible(t))
                                            for (var n = 0; n < e.data.length; n++)
                                                if (e.data[n].inRange(i.x, i.y)) return e.data[n]
                                    }
                            }.call(a);
                        return o ? (e.each(a.data.datasets, function(t, e) {
                            if (a.isDatasetVisible(e)) {
                                var i = a.getDatasetMeta(e),
                                    r = i.data[o._index];
                                r && !r._view.skip && n.push(r)
                            }
                        }, a), n) : n
                    },
                    getElementsAtXAxis: function(t) {
                        var a = this,
                            i = e.getRelativePosition(t, a.chart),
                            n = [],
                            o = function() {
                                if (a.data.datasets)
                                    for (var t = 0; t < a.data.datasets.length; t++) {
                                        var e = a.getDatasetMeta(t);
                                        if (a.isDatasetVisible(t))
                                            for (var n = 0; n < e.data.length; n++)
                                                if (e.data[n].inLabelRange(i.x, i.y)) return e.data[n]
                                    }
                            }.call(a);
                        return o ? (e.each(a.data.datasets, function(t, i) {
                            if (a.isDatasetVisible(i)) {
                                var r = a.getDatasetMeta(i),
                                    l = e.findIndex(r.data, function(t) {
                                        return o._model.x === t._model.x
                                    }); - 1 === l || r.data[l]._view.skip || n.push(r.data[l])
                            }
                        }, a), n) : n
                    },
                    getElementsAtEventForMode: function(t, e) {
                        var a = this;
                        switch (e) {
                            case "single":
                                return a.getElementAtEvent(t);
                            case "label":
                                return a.getElementsAtEvent(t);
                            case "dataset":
                                return a.getDatasetAtEvent(t);
                            case "x-axis":
                                return a.getElementsAtXAxis(t);
                            default:
                                return t
                        }
                    },
                    getDatasetAtEvent: function(t) {
                        var e = this.getElementAtEvent(t);
                        return e.length > 0 && (e = this.getDatasetMeta(e[0]._datasetIndex).data), e
                    },
                    getDatasetMeta: function(t) {
                        var e = this,
                            a = e.data.datasets[t];
                        a._meta || (a._meta = {});
                        var i = a._meta[e.id];
                        return i || (i = a._meta[e.id] = {
                            type: null,
                            data: [],
                            dataset: null,
                            controller: null,
                            hidden: null,
                            xAxisID: null,
                            yAxisID: null
                        }), i
                    },
                    getVisibleDatasetCount: function() {
                        for (var t = 0, e = 0, a = this.data.datasets.length; a > e; ++e) this.isDatasetVisible(e) && t++;
                        return t
                    },
                    isDatasetVisible: function(t) {
                        var e = this.getDatasetMeta(t);
                        return "boolean" == typeof e.hidden ? !e.hidden : !this.data.datasets[t].hidden;
                    },
                    generateLegend: function() {
                        return this.options.legendCallback(this)
                    },
                    destroy: function() {
                        var a = this;
                        a.stop(), a.clear(), e.unbindEvents(a, a.events), e.removeResizeListener(a.chart.canvas.parentNode);
                        var i = a.chart.canvas;
                        i.width = a.chart.width, i.height = a.chart.height, void 0 !== a.chart.originalDevicePixelRatio && a.chart.ctx.scale(1 / a.chart.originalDevicePixelRatio, 1 / a.chart.originalDevicePixelRatio), i.style.width = a.chart.originalCanvasStyleWidth, i.style.height = a.chart.originalCanvasStyleHeight, t.plugins.notify("destroy", [a]), delete t.instances[a.id]
                    },
                    toBase64Image: function() {
                        return this.chart.canvas.toDataURL.apply(this.chart.canvas, arguments)
                    },
                    initToolTip: function() {
                        var e = this;
                        e.tooltip = new t.Tooltip({
                            _chart: e.chart,
                            _chartInstance: e,
                            _data: e.data,
                            _options: e.options.tooltips
                        }, e)
                    },
                    bindEvents: function() {
                        var t = this;
                        e.bindEvents(t, t.options.events, function(e) {
                            t.eventHandler(e)
                        })
                    },
                    updateHoverStyle: function(t, e, a) {
                        var i, n, o, r = a ? "setHoverStyle" : "removeHoverStyle";
                        switch (e) {
                            case "single":
                                t = [t[0]];
                                break;
                            case "label":
                            case "dataset":
                            case "x-axis":
                                break;
                            default:
                                return
                        }
                        for (n = 0, o = t.length; o > n; ++n) i = t[n], i && this.getDatasetMeta(i._datasetIndex).controller[r](i)
                    },
                    eventHandler: function(t) {
                        var a = this,
                            i = a.tooltip,
                            n = a.options || {},
                            o = n.hover,
                            r = n.tooltips;
                        return a.lastActive = a.lastActive || [], a.lastTooltipActive = a.lastTooltipActive || [], "mouseout" === t.type ? (a.active = [], a.tooltipActive = []) : (a.active = a.getElementsAtEventForMode(t, o.mode), a.tooltipActive = a.getElementsAtEventForMode(t, r.mode)), o.onHover && o.onHover.call(a, a.active), ("mouseup" === t.type || "click" === t.type) && (n.onClick && n.onClick.call(a, t, a.active), a.legend && a.legend.handleEvent && a.legend.handleEvent(t)), a.lastActive.length && a.updateHoverStyle(a.lastActive, o.mode, !1), a.active.length && o.mode && a.updateHoverStyle(a.active, o.mode, !0), (r.enabled || r.custom) && (i.initialize(), i._active = a.tooltipActive, i.update(!0)), i.pivot(), a.animating || e.arrayEquals(a.active, a.lastActive) && e.arrayEquals(a.tooltipActive, a.lastTooltipActive) || (a.stop(), (r.enabled || r.custom) && i.update(!0), a.render(o.animationDuration, !0)), a.lastActive = a.active, a.lastTooltipActive = a.tooltipActive, a
                    }
                })
            }
        }, {}],
        24: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                var e = t.helpers,
                    a = e.noop;
                t.DatasetController = function(t, e) {
                    this.initialize.call(this, t, e)
                }, e.extend(t.DatasetController.prototype, {
                    datasetElementType: null,
                    dataElementType: null,
                    initialize: function(t, e) {
                        var a = this;
                        a.chart = t, a.index = e, a.linkScales(), a.addElements()
                    },
                    updateIndex: function(t) {
                        this.index = t
                    },
                    linkScales: function() {
                        var t = this,
                            e = t.getMeta(),
                            a = t.getDataset();
                        null === e.xAxisID && (e.xAxisID = a.xAxisID || t.chart.options.scales.xAxes[0].id), null === e.yAxisID && (e.yAxisID = a.yAxisID || t.chart.options.scales.yAxes[0].id)
                    },
                    getDataset: function() {
                        return this.chart.data.datasets[this.index]
                    },
                    getMeta: function() {
                        return this.chart.getDatasetMeta(this.index)
                    },
                    getScaleForId: function(t) {
                        return this.chart.scales[t]
                    },
                    reset: function() {
                        this.update(!0)
                    },
                    createMetaDataset: function() {
                        var t = this,
                            e = t.datasetElementType;
                        return e && new e({
                            _chart: t.chart.chart,
                            _datasetIndex: t.index
                        })
                    },
                    createMetaData: function(t) {
                        var e = this,
                            a = e.dataElementType;
                        return a && new a({
                            _chart: e.chart.chart,
                            _datasetIndex: e.index,
                            _index: t
                        })
                    },
                    addElements: function() {
                        var t, e, a = this,
                            i = a.getMeta(),
                            n = a.getDataset().data || [],
                            o = i.data;
                        for (t = 0, e = n.length; e > t; ++t) o[t] = o[t] || a.createMetaData(i, t);
                        i.dataset = i.dataset || a.createMetaDataset()
                    },
                    addElementAndReset: function(t) {
                        var e = this,
                            a = e.createMetaData(t);
                        e.getMeta().data.splice(t, 0, a), e.updateElement(a, t, !0)
                    },
                    buildOrUpdateElements: function() {
                        var t = this.getMeta(),
                            e = t.data,
                            a = this.getDataset().data.length,
                            i = e.length;
                        if (i > a) e.splice(a, i - a);
                        else if (a > i)
                            for (var n = i; a > n; ++n) this.addElementAndReset(n)
                    },
                    update: a,
                    draw: function(t) {
                        var a = t || 1;
                        e.each(this.getMeta().data, function(t) {
                            t.transition(a).draw()
                        })
                    },
                    removeHoverStyle: function(t, a) {
                        var i = this.chart.data.datasets[t._datasetIndex],
                            n = t._index,
                            o = t.custom || {},
                            r = e.getValueAtIndexOrDefault,
                            l = t._model;
                        l.backgroundColor = o.backgroundColor ? o.backgroundColor : r(i.backgroundColor, n, a.backgroundColor), l.borderColor = o.borderColor ? o.borderColor : r(i.borderColor, n, a.borderColor), l.borderWidth = o.borderWidth ? o.borderWidth : r(i.borderWidth, n, a.borderWidth)
                    },
                    setHoverStyle: function(t) {
                        var a = this.chart.data.datasets[t._datasetIndex],
                            i = t._index,
                            n = t.custom || {},
                            o = e.getValueAtIndexOrDefault,
                            r = e.getHoverColor,
                            l = t._model;
                        l.backgroundColor = n.hoverBackgroundColor ? n.hoverBackgroundColor : o(a.hoverBackgroundColor, i, r(l.backgroundColor)), l.borderColor = n.hoverBorderColor ? n.hoverBorderColor : o(a.hoverBorderColor, i, r(l.borderColor)), l.borderWidth = n.hoverBorderWidth ? n.hoverBorderWidth : o(a.hoverBorderWidth, i, l.borderWidth)
                    }
                }), t.DatasetController.extend = e.inherits
            }
        }, {}],
        25: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                var e = t.helpers;
                t.elements = {}, t.Element = function(t) {
                    e.extend(this, t), this.initialize.apply(this, arguments)
                }, e.extend(t.Element.prototype, {
                    initialize: function() {
                        this.hidden = !1
                    },
                    pivot: function() {
                        var t = this;
                        return t._view || (t._view = e.clone(t._model)), t._start = e.clone(t._view), t
                    },
                    transition: function(t) {
                        var a = this;
                        return a._view || (a._view = e.clone(a._model)), 1 === t ? (a._view = a._model, a._start = null, a) : (a._start || a.pivot(), e.each(a._model, function(i, n) {
                            if ("_" === n[0]);
                            else if (a._view.hasOwnProperty(n))
                                if (i === a._view[n]);
                                else if ("string" == typeof i) try {
                                var o = e.color(a._model[n]).mix(e.color(a._start[n]), t);
                                a._view[n] = o.rgbString()
                            } catch (r) {
                                a._view[n] = i
                            } else if ("number" == typeof i) {
                                var l = void 0 !== a._start[n] && isNaN(a._start[n]) === !1 ? a._start[n] : 0;
                                a._view[n] = (a._model[n] - l) * t + l
                            } else a._view[n] = i;
                            else "number" != typeof i || isNaN(a._view[n]) ? a._view[n] = i : a._view[n] = i * t
                        }, a), a)
                    },
                    tooltipPosition: function() {
                        return {
                            x: this._model.x,
                            y: this._model.y
                        }
                    },
                    hasValue: function() {
                        return e.isNumber(this._model.x) && e.isNumber(this._model.y)
                    }
                }), t.Element.extend = e.inherits
            }
        }, {}],
        26: [function(t, e, a) {
            "use strict";
            var i = t(3);
            e.exports = function(t) {
                function e(t, e, a) {
                    var i;
                    return "string" == typeof t ? (i = parseInt(t, 10), -1 != t.indexOf("%") && (i = i / 100 * e.parentNode[a])) : i = t, i
                }

                function a(t) {
                    return void 0 !== t && null !== t && "none" !== t
                }

                function n(t, i, n) {
                    var o = document.defaultView,
                        r = t.parentNode,
                        l = o.getComputedStyle(t)[i],
                        s = o.getComputedStyle(r)[i],
                        d = a(l),
                        u = a(s),
                        c = Number.POSITIVE_INFINITY;
                    return d || u ? Math.min(d ? e(l, t, n) : c, u ? e(s, r, n) : c) : "none"
                }
                var o = t.helpers = {};
                o.each = function(t, e, a, i) {
                    var n, r;
                    if (o.isArray(t))
                        if (r = t.length, i)
                            for (n = r - 1; n >= 0; n--) e.call(a, t[n], n);
                        else
                            for (n = 0; r > n; n++) e.call(a, t[n], n);
                    else if ("object" == typeof t) {
                        var l = Object.keys(t);
                        for (r = l.length, n = 0; r > n; n++) e.call(a, t[l[n]], l[n])
                    }
                }, o.clone = function(t) {
                    var e = {};
                    return o.each(t, function(t, a) {
                        o.isArray(t) ? e[a] = t.slice(0) : "object" == typeof t && null !== t ? e[a] = o.clone(t) : e[a] = t
                    }), e
                }, o.extend = function(t) {
                    for (var e = function(e, a) {
                            t[a] = e
                        }, a = 1, i = arguments.length; i > a; a++) o.each(arguments[a], e);
                    return t
                }, o.configMerge = function(e) {
                    var a = o.clone(e);
                    return o.each(Array.prototype.slice.call(arguments, 1), function(e) {
                        o.each(e, function(e, i) {
                            if ("scales" === i) a[i] = o.scaleMerge(a.hasOwnProperty(i) ? a[i] : {}, e);
                            else if ("scale" === i) a[i] = o.configMerge(a.hasOwnProperty(i) ? a[i] : {}, t.scaleService.getScaleDefaults(e.type), e);
                            else if (a.hasOwnProperty(i) && o.isArray(a[i]) && o.isArray(e)) {
                                var n = a[i];
                                o.each(e, function(t, e) {
                                    e < n.length ? "object" == typeof n[e] && null !== n[e] && "object" == typeof t && null !== t ? n[e] = o.configMerge(n[e], t) : n[e] = t : n.push(t)
                                })
                            } else a.hasOwnProperty(i) && "object" == typeof a[i] && null !== a[i] && "object" == typeof e ? a[i] = o.configMerge(a[i], e) : a[i] = e
                        })
                    }), a
                }, o.scaleMerge = function(e, a) {
                    var i = o.clone(e);
                    return o.each(a, function(e, a) {
                        "xAxes" === a || "yAxes" === a ? i.hasOwnProperty(a) ? o.each(e, function(e, n) {
                            var r = o.getValueOrDefault(e.type, "xAxes" === a ? "category" : "linear"),
                                l = t.scaleService.getScaleDefaults(r);
                            n >= i[a].length || !i[a][n].type ? i[a].push(o.configMerge(l, e)) : e.type && e.type !== i[a][n].type ? i[a][n] = o.configMerge(i[a][n], l, e) : i[a][n] = o.configMerge(i[a][n], e)
                        }) : (i[a] = [], o.each(e, function(e) {
                            var n = o.getValueOrDefault(e.type, "xAxes" === a ? "category" : "linear");
                            i[a].push(o.configMerge(t.scaleService.getScaleDefaults(n), e))
                        })) : i.hasOwnProperty(a) && "object" == typeof i[a] && null !== i[a] && "object" == typeof e ? i[a] = o.configMerge(i[a], e) : i[a] = e
                    }), i
                }, o.getValueAtIndexOrDefault = function(t, e, a) {
                    return void 0 === t || null === t ? a : o.isArray(t) ? e < t.length ? t[e] : a : t
                }, o.getValueOrDefault = function(t, e) {
                    return void 0 === t ? e : t
                }, o.indexOf = Array.prototype.indexOf ? function(t, e) {
                    return t.indexOf(e)
                } : function(t, e) {
                    for (var a = 0, i = t.length; i > a; ++a)
                        if (t[a] === e) return a;
                    return -1
                }, o.where = function(t, e) {
                    if (o.isArray(t) && Array.prototype.filter) return t.filter(e);
                    var a = [];
                    return o.each(t, function(t) {
                        e(t) && a.push(t)
                    }), a
                }, o.findIndex = Array.prototype.findIndex ? function(t, e, a) {
                    return t.findIndex(e, a)
                } : function(t, e, a) {
                    a = void 0 === a ? t : a;
                    for (var i = 0, n = t.length; n > i; ++i)
                        if (e.call(a, t[i], i, t)) return i;
                    return -1
                }, o.findNextWhere = function(t, e, a) {
                    (void 0 === a || null === a) && (a = -1);
                    for (var i = a + 1; i < t.length; i++) {
                        var n = t[i];
                        if (e(n)) return n
                    }
                }, o.findPreviousWhere = function(t, e, a) {
                    (void 0 === a || null === a) && (a = t.length);
                    for (var i = a - 1; i >= 0; i--) {
                        var n = t[i];
                        if (e(n)) return n
                    }
                }, o.inherits = function(t) {
                    var e = this,
                        a = t && t.hasOwnProperty("constructor") ? t.constructor : function() {
                            return e.apply(this, arguments)
                        },
                        i = function() {
                            this.constructor = a
                        };
                    return i.prototype = e.prototype, a.prototype = new i, a.extend = o.inherits, t && o.extend(a.prototype, t), a.__super__ = e.prototype, a
                }, o.noop = function() {}, o.uid = function() {
                    var t = 0;
                    return function() {
                        return t++
                    }
                }(), o.isNumber = function(t) {
                    return !isNaN(parseFloat(t)) && isFinite(t)
                }, o.almostEquals = function(t, e, a) {
                    return Math.abs(t - e) < a
                }, o.max = function(t) {
                    return t.reduce(function(t, e) {
                        return isNaN(e) ? t : Math.max(t, e)
                    }, Number.NEGATIVE_INFINITY)
                }, o.min = function(t) {
                    return t.reduce(function(t, e) {
                        return isNaN(e) ? t : Math.min(t, e)
                    }, Number.POSITIVE_INFINITY)
                }, o.sign = Math.sign ? function(t) {
                    return Math.sign(t)
                } : function(t) {
                    return t = +t, 0 === t || isNaN(t) ? t : t > 0 ? 1 : -1
                }, o.log10 = Math.log10 ? function(t) {
                    return Math.log10(t)
                } : function(t) {
                    return Math.log(t) / Math.LN10
                }, o.toRadians = function(t) {
                    return t * (Math.PI / 180)
                }, o.toDegrees = function(t) {
                    return t * (180 / Math.PI)
                }, o.getAngleFromPoint = function(t, e) {
                    var a = e.x - t.x,
                        i = e.y - t.y,
                        n = Math.sqrt(a * a + i * i),
                        o = Math.atan2(i, a);
                    return o < -.5 * Math.PI && (o += 2 * Math.PI), {
                        angle: o,
                        distance: n
                    }
                }, o.aliasPixel = function(t) {
                    return t % 2 === 0 ? 0 : .5
                }, o.splineCurve = function(t, e, a, i) {
                    var n = t.skip ? e : t,
                        o = e,
                        r = a.skip ? e : a,
                        l = Math.sqrt(Math.pow(o.x - n.x, 2) + Math.pow(o.y - n.y, 2)),
                        s = Math.sqrt(Math.pow(r.x - o.x, 2) + Math.pow(r.y - o.y, 2)),
                        d = l / (l + s),
                        u = s / (l + s);
                    d = isNaN(d) ? 0 : d, u = isNaN(u) ? 0 : u;
                    var c = i * d,
                        h = i * u;
                    return {
                        previous: {
                            x: o.x - c * (r.x - n.x),
                            y: o.y - c * (r.y - n.y)
                        },
                        next: {
                            x: o.x + h * (r.x - n.x),
                            y: o.y + h * (r.y - n.y)
                        }
                    }
                }, o.nextItem = function(t, e, a) {
                    return a ? e >= t.length - 1 ? t[0] : t[e + 1] : e >= t.length - 1 ? t[t.length - 1] : t[e + 1]
                }, o.previousItem = function(t, e, a) {
                    return a ? 0 >= e ? t[t.length - 1] : t[e - 1] : 0 >= e ? t[0] : t[e - 1]
                }, o.niceNum = function(t, e) {
                    var a, i = Math.floor(o.log10(t)),
                        n = t / Math.pow(10, i);
                    return a = e ? 1.5 > n ? 1 : 3 > n ? 2 : 7 > n ? 5 : 10 : 1 >= n ? 1 : 2 >= n ? 2 : 5 >= n ? 5 : 10, a * Math.pow(10, i)
                };
                var r = o.easingEffects = {
                    linear: function(t) {
                        return t
                    },
                    easeInQuad: function(t) {
                        return t * t
                    },
                    easeOutQuad: function(t) {
                        return -1 * t * (t - 2)
                    },
                    easeInOutQuad: function(t) {
                        return (t /= .5) < 1 ? .5 * t * t : -0.5 * (--t * (t - 2) - 1)
                    },
                    easeInCubic: function(t) {
                        return t * t * t
                    },
                    easeOutCubic: function(t) {
                        return 1 * ((t = t / 1 - 1) * t * t + 1)
                    },
                    easeInOutCubic: function(t) {
                        return (t /= .5) < 1 ? .5 * t * t * t : .5 * ((t -= 2) * t * t + 2)
                    },
                    easeInQuart: function(t) {
                        return t * t * t * t
                    },
                    easeOutQuart: function(t) {
                        return -1 * ((t = t / 1 - 1) * t * t * t - 1)
                    },
                    easeInOutQuart: function(t) {
                        return (t /= .5) < 1 ? .5 * t * t * t * t : -0.5 * ((t -= 2) * t * t * t - 2)
                    },
                    easeInQuint: function(t) {
                        return 1 * (t /= 1) * t * t * t * t
                    },
                    easeOutQuint: function(t) {
                        return 1 * ((t = t / 1 - 1) * t * t * t * t + 1)
                    },
                    easeInOutQuint: function(t) {
                        return (t /= .5) < 1 ? .5 * t * t * t * t * t : .5 * ((t -= 2) * t * t * t * t + 2)
                    },
                    easeInSine: function(t) {
                        return -1 * Math.cos(t / 1 * (Math.PI / 2)) + 1
                    },
                    easeOutSine: function(t) {
                        return 1 * Math.sin(t / 1 * (Math.PI / 2))
                    },
                    easeInOutSine: function(t) {
                        return -0.5 * (Math.cos(Math.PI * t / 1) - 1)
                    },
                    easeInExpo: function(t) {
                        return 0 === t ? 1 : 1 * Math.pow(2, 10 * (t / 1 - 1))
                    },
                    easeOutExpo: function(t) {
                        return 1 === t ? 1 : 1 * (-Math.pow(2, -10 * t / 1) + 1)
                    },
                    easeInOutExpo: function(t) {
                        return 0 === t ? 0 : 1 === t ? 1 : (t /= .5) < 1 ? .5 * Math.pow(2, 10 * (t - 1)) : .5 * (-Math.pow(2, -10 * --t) + 2)
                    },
                    easeInCirc: function(t) {
                        return t >= 1 ? t : -1 * (Math.sqrt(1 - (t /= 1) * t) - 1)
                    },
                    easeOutCirc: function(t) {
                        return 1 * Math.sqrt(1 - (t = t / 1 - 1) * t)
                    },
                    easeInOutCirc: function(t) {
                        return (t /= .5) < 1 ? -0.5 * (Math.sqrt(1 - t * t) - 1) : .5 * (Math.sqrt(1 - (t -= 2) * t) + 1)
                    },
                    easeInElastic: function(t) {
                        var e = 1.70158,
                            a = 0,
                            i = 1;
                        return 0 === t ? 0 : 1 === (t /= 1) ? 1 : (a || (a = .3), i < Math.abs(1) ? (i = 1, e = a / 4) : e = a / (2 * Math.PI) * Math.asin(1 / i), -(i * Math.pow(2, 10 * (t -= 1)) * Math.sin((1 * t - e) * (2 * Math.PI) / a)))
                    },
                    easeOutElastic: function(t) {
                        var e = 1.70158,
                            a = 0,
                            i = 1;
                        return 0 === t ? 0 : 1 === (t /= 1) ? 1 : (a || (a = .3), i < Math.abs(1) ? (i = 1, e = a / 4) : e = a / (2 * Math.PI) * Math.asin(1 / i), i * Math.pow(2, -10 * t) * Math.sin((1 * t - e) * (2 * Math.PI) / a) + 1)
                    },
                    easeInOutElastic: function(t) {
                        var e = 1.70158,
                            a = 0,
                            i = 1;
                        return 0 === t ? 0 : 2 === (t /= .5) ? 1 : (a || (a = 1 * (.3 * 1.5)), i < Math.abs(1) ? (i = 1, e = a / 4) : e = a / (2 * Math.PI) * Math.asin(1 / i), 1 > t ? -.5 * (i * Math.pow(2, 10 * (t -= 1)) * Math.sin((1 * t - e) * (2 * Math.PI) / a)) : i * Math.pow(2, -10 * (t -= 1)) * Math.sin((1 * t - e) * (2 * Math.PI) / a) * .5 + 1)
                    },
                    easeInBack: function(t) {
                        var e = 1.70158;
                        return 1 * (t /= 1) * t * ((e + 1) * t - e)
                    },
                    easeOutBack: function(t) {
                        var e = 1.70158;
                        return 1 * ((t = t / 1 - 1) * t * ((e + 1) * t + e) + 1)
                    },
                    easeInOutBack: function(t) {
                        var e = 1.70158;
                        return (t /= .5) < 1 ? .5 * (t * t * (((e *= 1.525) + 1) * t - e)) : .5 * ((t -= 2) * t * (((e *= 1.525) + 1) * t + e) + 2)
                    },
                    easeInBounce: function(t) {
                        return 1 - r.easeOutBounce(1 - t)
                    },
                    easeOutBounce: function(t) {
                        return (t /= 1) < 1 / 2.75 ? 1 * (7.5625 * t * t) : 2 / 2.75 > t ? 1 * (7.5625 * (t -= 1.5 / 2.75) * t + .75) : 2.5 / 2.75 > t ? 1 * (7.5625 * (t -= 2.25 / 2.75) * t + .9375) : 1 * (7.5625 * (t -= 2.625 / 2.75) * t + .984375)
                    },
                    easeInOutBounce: function(t) {
                        return .5 > t ? .5 * r.easeInBounce(2 * t) : .5 * r.easeOutBounce(2 * t - 1) + .5
                    }
                };
                o.requestAnimFrame = function() {
                    return window.requestAnimationFrame || window.webkitRequestAnimationFrame || window.mozRequestAnimationFrame || window.oRequestAnimationFrame || window.msRequestAnimationFrame || function(t) {
                        return window.setTimeout(t, 1e3 / 60)
                    }
                }(), o.cancelAnimFrame = function() {
                    return window.cancelAnimationFrame || window.webkitCancelAnimationFrame || window.mozCancelAnimationFrame || window.oCancelAnimationFrame || window.msCancelAnimationFrame || function(t) {
                        return window.clearTimeout(t, 1e3 / 60)
                    }
                }(), o.getRelativePosition = function(t, e) {
                    var a, i, n = t.originalEvent || t,
                        r = t.currentTarget || t.srcElement,
                        l = r.getBoundingClientRect(),
                        s = n.touches;
                    s && s.length > 0 ? (a = s[0].clientX, i = s[0].clientY) : (a = n.clientX, i = n.clientY);
                    var d = parseFloat(o.getStyle(r, "padding-left")),
                        u = parseFloat(o.getStyle(r, "padding-top")),
                        c = parseFloat(o.getStyle(r, "padding-right")),
                        h = parseFloat(o.getStyle(r, "padding-bottom")),
                        f = l.right - l.left - d - c,
                        g = l.bottom - l.top - u - h;
                    return a = Math.round((a - l.left - d) / f * r.width / e.currentDevicePixelRatio), i = Math.round((i - l.top - u) / g * r.height / e.currentDevicePixelRatio), {
                        x: a,
                        y: i
                    }
                }, o.addEvent = function(t, e, a) {
                    t.addEventListener ? t.addEventListener(e, a) : t.attachEvent ? t.attachEvent("on" + e, a) : t["on" + e] = a
                }, o.removeEvent = function(t, e, a) {
                    t.removeEventListener ? t.removeEventListener(e, a, !1) : t.detachEvent ? t.detachEvent("on" + e, a) : t["on" + e] = o.noop
                }, o.bindEvents = function(t, e, a) {
                    var i = t.events = t.events || {};
                    o.each(e, function(e) {
                        i[e] = function() {
                            a.apply(t, arguments)
                        }, o.addEvent(t.chart.canvas, e, i[e])
                    })
                }, o.unbindEvents = function(t, e) {
                    var a = t.chart.canvas;
                    o.each(e, function(t, e) {
                        o.removeEvent(a, e, t)
                    })
                }, o.getConstraintWidth = function(t) {
                    return n(t, "max-width", "clientWidth")
                }, o.getConstraintHeight = function(t) {
                    return n(t, "max-height", "clientHeight")
                }, o.getMaximumWidth = function(t) {
                    var e = t.parentNode,
                        a = parseInt(o.getStyle(e, "padding-left")) + parseInt(o.getStyle(e, "padding-right")),
                        i = e.clientWidth - a,
                        n = o.getConstraintWidth(t);
                    return isNaN(n) ? i : Math.min(i, n)
                }, o.getMaximumHeight = function(t) {
                    var e = t.parentNode,
                        a = parseInt(o.getStyle(e, "padding-top")) + parseInt(o.getStyle(e, "padding-bottom")),
                        i = e.clientHeight - a,
                        n = o.getConstraintHeight(t);
                    return isNaN(n) ? i : Math.min(i, n)
                }, o.getStyle = function(t, e) {
                    return t.currentStyle ? t.currentStyle[e] : document.defaultView.getComputedStyle(t, null).getPropertyValue(e)
                }, o.retinaScale = function(t) {
                    var e = t.ctx,
                        a = t.canvas,
                        i = a.width,
                        n = a.height,
                        o = t.currentDevicePixelRatio = window.devicePixelRatio || 1;
                    1 !== o && (a.height = n * o, a.width = i * o, e.scale(o, o), t.originalDevicePixelRatio = t.originalDevicePixelRatio || o), a.style.width = i + "px", a.style.height = n + "px"
                }, o.clear = function(t) {
                    t.ctx.clearRect(0, 0, t.width, t.height)
                }, o.fontString = function(t, e, a) {
                    return e + " " + t + "px " + a
                }, o.longestText = function(t, e, a, i) {
                    i = i || {};
                    var n = i.data = i.data || {},
                        r = i.garbageCollect = i.garbageCollect || [];
                    i.font !== e && (n = i.data = {}, r = i.garbageCollect = [], i.font = e), t.font = e;
                    var l = 0;
                    o.each(a, function(e) {
                        void 0 !== e && null !== e && o.isArray(e) !== !0 ? l = o.measureText(t, n, r, l, e) : o.isArray(e) && o.each(e, function(e) {
                            void 0 === e || null === e || o.isArray(e) || (l = o.measureText(t, n, r, l, e))
                        })
                    });
                    var s = r.length / 2;
                    if (s > a.length) {
                        for (var d = 0; s > d; d++) delete n[r[d]];
                        r.splice(0, s)
                    }
                    return l
                }, o.measureText = function(t, e, a, i, n) {
                    var o = e[n];
                    return o || (o = e[n] = t.measureText(n).width, a.push(n)), o > i && (i = o), i
                }, o.numberOfLabelLines = function(t) {
                    var e = 1;
                    return o.each(t, function(t) {
                        o.isArray(t) && t.length > e && (e = t.length)
                    }), e
                }, o.drawRoundedRectangle = function(t, e, a, i, n, o) {
                    t.beginPath(), t.moveTo(e + o, a), t.lineTo(e + i - o, a), t.quadraticCurveTo(e + i, a, e + i, a + o), t.lineTo(e + i, a + n - o), t.quadraticCurveTo(e + i, a + n, e + i - o, a + n), t.lineTo(e + o, a + n), t.quadraticCurveTo(e, a + n, e, a + n - o), t.lineTo(e, a + o), t.quadraticCurveTo(e, a, e + o, a), t.closePath()
                }, o.color = function(e) {
                    return i ? i(e instanceof CanvasGradient ? t.defaults.global.defaultColor : e) : (console.log("Color.js not found!"), e)
                }, o.addResizeListener = function(t, e) {
                    var a = document.createElement("iframe"),
                        i = "chartjs-hidden-iframe";
                    a.classlist ? a.classlist.add(i) : a.setAttribute("class", i);
                    var n = a.style;
                    n.width = "100%", n.display = "block", n.border = 0, n.height = 0, n.margin = 0, n.position = "absolute", n.left = 0, n.right = 0, n.top = 0, n.bottom = 0, t.insertBefore(a, t.firstChild), (a.contentWindow || a).onresize = function() {
                        e && e()
                    }
                }, o.removeResizeListener = function(t) {
                    var e = t.querySelector(".chartjs-hidden-iframe");
                    e && e.parentNode.removeChild(e)
                }, o.isArray = Array.isArray ? function(t) {
                    return Array.isArray(t)
                } : function(t) {
                    return "[object Array]" === Object.prototype.toString.call(t)
                }, o.arrayEquals = function(t, e) {
                    var a, i, n, r;
                    if (!t || !e || t.length != e.length) return !1;
                    for (a = 0, i = t.length; i > a; ++a)
                        if (n = t[a], r = e[a], n instanceof Array && r instanceof Array) {
                            if (!o.arrayEquals(n, r)) return !1
                        } else if (n != r) return !1;
                    return !0
                }, o.callCallback = function(t, e, a) {
                    t && "function" == typeof t.call && t.apply(a, e)
                }, o.getHoverColor = function(t) {
                    return t instanceof CanvasPattern ? t : o.color(t).saturate(.5).darken(.1).rgbString()
                }
            }
        }, {
            3: 3
        }],
        27: [function(t, e, a) {
            "use strict";
            e.exports = function() {
                var t = function(e, a) {
                    var i = this,
                        n = t.helpers;
                    return i.config = a || {
                        data: {
                            datasets: []
                        }
                    }, e.length && e[0].getContext && (e = e[0]), e.getContext && (e = e.getContext("2d")), i.ctx = e, i.canvas = e.canvas, e.canvas.style.display = e.canvas.style.display || "block", i.width = e.canvas.width || parseInt(n.getStyle(e.canvas, "width"), 10) || n.getMaximumWidth(e.canvas), i.height = e.canvas.height || parseInt(n.getStyle(e.canvas, "height"), 10) || n.getMaximumHeight(e.canvas), i.aspectRatio = i.width / i.height, (isNaN(i.aspectRatio) || isFinite(i.aspectRatio) === !1) && (i.aspectRatio = void 0 !== a.aspectRatio ? a.aspectRatio : 2), i.originalCanvasStyleWidth = e.canvas.style.width, i.originalCanvasStyleHeight = e.canvas.style.height, n.retinaScale(i), i.controller = new t.Controller(i), n.addResizeListener(e.canvas.parentNode, function() {
                        i.controller && i.controller.config.options.responsive && i.controller.resize()
                    }), i.controller ? i.controller : i
                };
                return t.defaults = {
                    global: {
                        responsive: !0,
                        responsiveAnimationDuration: 0,
                        maintainAspectRatio: !0,
                        events: ["mousemove", "mouseout", "click", "touchstart", "touchmove"],
                        hover: {
                            onHover: null,
                            mode: "single",
                            animationDuration: 400
                        },
                        onClick: null,
                        defaultColor: "rgba(0,0,0,0.1)",
                        defaultFontColor: "#666",
                        defaultFontFamily: "'Helvetica Neue', 'Helvetica', 'Arial', sans-serif",
                        defaultFontSize: 12,
                        defaultFontStyle: "normal",
                        showLines: !0,
                        elements: {},
                        legendCallback: function(t) {
                            var e = [];
                            e.push('<ul class="' + t.id + '-legend">');
                            for (var a = 0; a < t.data.datasets.length; a++) e.push('<li><span style="background-color:' + t.data.datasets[a].backgroundColor + '"></span>'), t.data.datasets[a].label && e.push(t.data.datasets[a].label), e.push("</li>");
                            return e.push("</ul>"), e.join("")
                        }
                    }
                }, t.Chart = t, t
            }
        }, {}],
        28: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                var e = t.helpers;
                t.layoutService = {
                    defaults: {},
                    addBox: function(t, e) {
                        t.boxes || (t.boxes = []), t.boxes.push(e)
                    },
                    removeBox: function(t, e) {
                        t.boxes && t.boxes.splice(t.boxes.indexOf(e), 1)
                    },
                    update: function(t, a, i) {
                        function n(t) {
                            var e, a = t.isHorizontal();
                            a ? (e = t.update(t.options.fullWidth ? p : k, y), S -= e.height) : (e = t.update(x, v), k -= e.width), w.push({
                                horizontal: a,
                                minSize: e,
                                box: t
                            })
                        }

                        function o(t) {
                            var a = e.findNextWhere(w, function(e) {
                                return e.box === t
                            });
                            if (a)
                                if (t.isHorizontal()) {
                                    var i = {
                                        left: C,
                                        right: M,
                                        top: 0,
                                        bottom: 0
                                    };
                                    t.update(t.options.fullWidth ? p : k, m / 2, i)
                                } else t.update(a.minSize.width, S)
                        }

                        function r(t) {
                            var a = e.findNextWhere(w, function(e) {
                                    return e.box === t
                                }),
                                i = {
                                    left: 0,
                                    right: 0,
                                    top: D,
                                    bottom: A
                                };
                            a && t.update(a.minSize.width, S, i)
                        }

                        function l(t) {
                            t.isHorizontal() ? (t.left = t.options.fullWidth ? s : C, t.right = t.options.fullWidth ? a - s : C + k, t.top = P, t.bottom = P + t.height, P = t.bottom) : (t.left = F, t.right = F + t.width, t.top = D, t.bottom = D + S, F = t.right)
                        }
                        if (t) {
                            var s = 0,
                                d = 0,
                                u = e.where(t.boxes, function(t) {
                                    return "left" === t.options.position
                                }),
                                c = e.where(t.boxes, function(t) {
                                    return "right" === t.options.position
                                }),
                                h = e.where(t.boxes, function(t) {
                                    return "top" === t.options.position
                                }),
                                f = e.where(t.boxes, function(t) {
                                    return "bottom" === t.options.position
                                }),
                                g = e.where(t.boxes, function(t) {
                                    return "chartArea" === t.options.position
                                });
                            h.sort(function(t, e) {
                                return (e.options.fullWidth ? 1 : 0) - (t.options.fullWidth ? 1 : 0)
                            }), f.sort(function(t, e) {
                                return (t.options.fullWidth ? 1 : 0) - (e.options.fullWidth ? 1 : 0)
                            });
                            var p = a - 2 * s,
                                m = i - 2 * d,
                                b = p / 2,
                                v = m / 2,
                                x = (a - b) / (u.length + c.length),
                                y = (i - v) / (h.length + f.length),
                                k = p,
                                S = m,
                                w = [];
                            e.each(u.concat(c, h, f), n);
                            var C = s,
                                M = s,
                                D = d,
                                A = d;
                            e.each(u.concat(c), o), e.each(u, function(t) {
                                C += t.width
                            }), e.each(c, function(t) {
                                M += t.width
                            }), e.each(h.concat(f), o), e.each(h, function(t) {
                                D += t.height
                            }), e.each(f, function(t) {
                                A += t.height
                            }), e.each(u.concat(c), r), C = s, M = s, D = d, A = d, e.each(u, function(t) {
                                C += t.width
                            }), e.each(c, function(t) {
                                M += t.width
                            }), e.each(h, function(t) {
                                D += t.height
                            }), e.each(f, function(t) {
                                A += t.height
                            });
                            var I = i - D - A,
                                T = a - C - M;
                            (T !== k || I !== S) && (e.each(u, function(t) {
                                t.height = I
                            }), e.each(c, function(t) {
                                t.height = I
                            }), e.each(h, function(t) {
                                t.options.fullWidth || (t.width = T)
                            }), e.each(f, function(t) {
                                t.options.fullWidth || (t.width = T)
                            }), S = I, k = T);
                            var F = s,
                                P = d;
                            e.each(u.concat(h), l), F += k, P += S, e.each(c, l), e.each(f, l), t.chartArea = {
                                left: C,
                                top: D,
                                right: C + k,
                                bottom: D + S
                            }, e.each(g, function(e) {
                                e.left = t.chartArea.left, e.top = t.chartArea.top, e.right = t.chartArea.right, e.bottom = t.chartArea.bottom, e.update(k, S)
                            })
                        }
                    }
                }
            }
        }, {}],
        29: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                var e = t.helpers,
                    a = e.noop;
                t.defaults.global.legend = {
                    display: !0,
                    position: "top",
                    fullWidth: !0,
                    reverse: !1,
                    onClick: function(t, e) {
                        var a = e.datasetIndex,
                            i = this.chart,
                            n = i.getDatasetMeta(a);
                        n.hidden = null === n.hidden ? !i.data.datasets[a].hidden : null, i.update()
                    },
                    labels: {
                        boxWidth: 40,
                        padding: 10,
                        generateLabels: function(t) {
                            var a = t.data;
                            return e.isArray(a.datasets) ? a.datasets.map(function(a, i) {
                                return {
                                    text: a.label,
                                    fillStyle: e.isArray(a.backgroundColor) ? a.backgroundColor[0] : a.backgroundColor,
                                    hidden: !t.isDatasetVisible(i),
                                    lineCap: a.borderCapStyle,
                                    lineDash: a.borderDash,
                                    lineDashOffset: a.borderDashOffset,
                                    lineJoin: a.borderJoinStyle,
                                    lineWidth: a.borderWidth,
                                    strokeStyle: a.borderColor,
                                    pointStyle: a.pointStyle,
                                    datasetIndex: i
                                }
                            }, this) : []
                        }
                    }
                }, t.Legend = t.Element.extend({
                    initialize: function(t) {
                        e.extend(this, t), this.legendHitBoxes = [], this.doughnutMode = !1
                    },
                    beforeUpdate: a,
                    update: function(t, e, a) {
                        var i = this;
                        return i.beforeUpdate(), i.maxWidth = t, i.maxHeight = e, i.margins = a, i.beforeSetDimensions(), i.setDimensions(), i.afterSetDimensions(), i.beforeBuildLabels(), i.buildLabels(), i.afterBuildLabels(), i.beforeFit(), i.fit(), i.afterFit(), i.afterUpdate(), i.minSize
                    },
                    afterUpdate: a,
                    beforeSetDimensions: a,
                    setDimensions: function() {
                        var t = this;
                        t.isHorizontal() ? (t.width = t.maxWidth, t.left = 0, t.right = t.width) : (t.height = t.maxHeight, t.top = 0, t.bottom = t.height), t.paddingLeft = 0, t.paddingTop = 0, t.paddingRight = 0, t.paddingBottom = 0, t.minSize = {
                            width: 0,
                            height: 0
                        }
                    },
                    afterSetDimensions: a,
                    beforeBuildLabels: a,
                    buildLabels: function() {
                        var t = this;
                        t.legendItems = t.options.labels.generateLabels.call(t, t.chart), t.options.reverse && t.legendItems.reverse()
                    },
                    afterBuildLabels: a,
                    beforeFit: a,
                    fit: function() {
                        var a = this,
                            i = a.options,
                            n = i.labels,
                            o = i.display,
                            r = a.ctx,
                            l = t.defaults.global,
                            s = e.getValueOrDefault,
                            d = s(n.fontSize, l.defaultFontSize),
                            u = s(n.fontStyle, l.defaultFontStyle),
                            c = s(n.fontFamily, l.defaultFontFamily),
                            h = e.fontString(d, u, c),
                            f = a.legendHitBoxes = [],
                            g = a.minSize,
                            p = a.isHorizontal();
                        if (p ? (g.width = a.maxWidth, g.height = o ? 10 : 0) : (g.width = o ? 10 : 0, g.height = a.maxHeight), o)
                            if (r.font = h, p) {
                                var m = a.lineWidths = [0],
                                    b = a.legendItems.length ? d + n.padding : 0;
                                r.textAlign = "left", r.textBaseline = "top", e.each(a.legendItems, function(t, e) {
                                    var i = n.usePointStyle ? d * Math.sqrt(2) : n.boxWidth,
                                        o = i + d / 2 + r.measureText(t.text).width;
                                    m[m.length - 1] + o + n.padding >= a.width && (b += d + n.padding, m[m.length] = a.left), f[e] = {
                                        left: 0,
                                        top: 0,
                                        width: o,
                                        height: d
                                    }, m[m.length - 1] += o + n.padding
                                }), g.height += b
                            } else {
                                var v = n.padding,
                                    x = a.columnWidths = [],
                                    y = n.padding,
                                    k = 0,
                                    S = 0,
                                    w = d + v;
                                e.each(a.legendItems, function(t, e) {
                                    var a = n.usePointStyle ? 2 * n.boxWidth : n.boxWidth,
                                        i = a + d / 2 + r.measureText(t.text).width;
                                    S + w > g.height && (y += k + n.padding, x.push(k), k = 0, S = 0), k = Math.max(k, i), S += w, f[e] = {
                                        left: 0,
                                        top: 0,
                                        width: i,
                                        height: d
                                    }
                                }), y += k, x.push(k), g.width += y
                            } a.width = g.width, a.height = g.height
                    },
                    afterFit: a,
                    isHorizontal: function() {
                        return "top" === this.options.position || "bottom" === this.options.position
                    },
                    draw: function() {
                        var a = this,
                            i = a.options,
                            n = i.labels,
                            o = t.defaults.global,
                            r = o.elements.line,
                            l = a.width,
                            s = a.lineWidths;
                        if (i.display) {
                            var d, u = a.ctx,
                                c = e.getValueOrDefault,
                                h = c(n.fontColor, o.defaultFontColor),
                                f = c(n.fontSize, o.defaultFontSize),
                                g = c(n.fontStyle, o.defaultFontStyle),
                                p = c(n.fontFamily, o.defaultFontFamily),
                                m = e.fontString(f, g, p);
                            u.textAlign = "left", u.textBaseline = "top", u.lineWidth = .5, u.strokeStyle = h, u.fillStyle = h, u.font = m;
                            var b = n.boxWidth,
                                v = a.legendHitBoxes,
                                x = function(e, a, n) {
                                    if (!(isNaN(b) || 0 >= b)) {
                                        if (u.save(), u.fillStyle = c(n.fillStyle, o.defaultColor), u.lineCap = c(n.lineCap, r.borderCapStyle), u.lineDashOffset = c(n.lineDashOffset, r.borderDashOffset), u.lineJoin = c(n.lineJoin, r.borderJoinStyle), u.lineWidth = c(n.lineWidth, r.borderWidth), u.strokeStyle = c(n.strokeStyle, o.defaultColor), u.setLineDash && u.setLineDash(c(n.lineDash, r.borderDash)), i.labels && i.labels.usePointStyle) {
                                            var l = f * Math.SQRT2 / 2,
                                                s = l / Math.SQRT2,
                                                d = e + s,
                                                h = a + s;
                                            t.canvasHelpers.drawPoint(u, n.pointStyle, l, d, h)
                                        } else u.strokeRect(e, a, b, f), u.fillRect(e, a, b, f);
                                        u.restore()
                                    }
                                },
                                y = function(t, e, a, i) {
                                    u.fillText(a.text, b + f / 2 + t, e), a.hidden && (u.beginPath(), u.lineWidth = 2, u.moveTo(b + f / 2 + t, e + f / 2), u.lineTo(b + f / 2 + t + i, e + f / 2), u.stroke())
                                },
                                k = a.isHorizontal();
                            d = k ? {
                                x: a.left + (l - s[0]) / 2,
                                y: a.top + n.padding,
                                line: 0
                            } : {
                                x: a.left + n.padding,
                                y: a.top + n.padding,
                                line: 0
                            };
                            var S = f + n.padding;
                            e.each(a.legendItems, function(t, e) {
                                var i = u.measureText(t.text).width,
                                    o = n.usePointStyle ? f + f / 2 + i : b + f / 2 + i,
                                    r = d.x,
                                    c = d.y;
                                k ? r + o >= l && (c = d.y += S, d.line++, r = d.x = a.left + (l - s[d.line]) / 2) : c + S > a.bottom && (r = d.x = r + a.columnWidths[d.line] + n.padding, c = d.y = a.top, d.line++), x(r, c, t), v[e].left = r, v[e].top = c, y(r, c, t, i), k ? d.x += o + n.padding : d.y += S
                            })
                        }
                    },
                    handleEvent: function(t) {
                        var a = this,
                            i = e.getRelativePosition(t, a.chart.chart),
                            n = i.x,
                            o = i.y,
                            r = a.options;
                        if (n >= a.left && n <= a.right && o >= a.top && o <= a.bottom)
                            for (var l = a.legendHitBoxes, s = 0; s < l.length; ++s) {
                                var d = l[s];
                                if (n >= d.left && n <= d.left + d.width && o >= d.top && o <= d.top + d.height) {
                                    r.onClick && r.onClick.call(a, t, a.legendItems[s]);
                                    break
                                }
                            }
                    }
                }), t.plugins.register({
                    beforeInit: function(e) {
                        var a = e.options,
                            i = a.legend;
                        i && (e.legend = new t.Legend({
                            ctx: e.chart.ctx,
                            options: i,
                            chart: e
                        }), t.layoutService.addBox(e, e.legend))
                    }
                })
            }
        }, {}],
        30: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                var e = t.helpers.noop;
                t.plugins = {
                    _plugins: [],
                    register: function(t) {
                        var e = this._plugins;
                        [].concat(t).forEach(function(t) {
                            -1 === e.indexOf(t) && e.push(t)
                        })
                    },
                    unregister: function(t) {
                        var e = this._plugins;
                        [].concat(t).forEach(function(t) {
                            var a = e.indexOf(t); - 1 !== a && e.splice(a, 1)
                        })
                    },
                    clear: function() {
                        this._plugins = []
                    },
                    count: function() {
                        return this._plugins.length
                    },
                    getAll: function() {
                        return this._plugins
                    },
                    notify: function(t, e) {
                        var a, i, n = this._plugins,
                            o = n.length;
                        for (a = 0; o > a; ++a)
                            if (i = n[a], "function" == typeof i[t] && i[t].apply(i, e || []) === !1) return !1;
                        return !0
                    }
                }, t.PluginBase = t.Element.extend({
                    beforeInit: e,
                    afterInit: e,
                    beforeUpdate: e,
                    afterUpdate: e,
                    beforeDraw: e,
                    afterDraw: e,
                    destroy: e
                }), t.pluginService = t.plugins
            }
        }, {}],
        31: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                var e = t.helpers;
                t.defaults.scale = {
                    display: !0,
                    position: "left",
                    gridLines: {
                        display: !0,
                        color: "rgba(0, 0, 0, 0.1)",
                        lineWidth: 1,
                        drawBorder: !0,
                        drawOnChartArea: !0,
                        drawTicks: !0,
                        tickMarkLength: 10,
                        zeroLineWidth: 1,
                        zeroLineColor: "rgba(0,0,0,0.25)",
                        offsetGridLines: !1
                    },
                    scaleLabel: {
                        labelString: "",
                        display: !1
                    },
                    ticks: {
                        beginAtZero: !1,
                        minRotation: 0,
                        maxRotation: 50,
                        mirror: !1,
                        padding: 10,
                        reverse: !1,
                        display: !0,
                        autoSkip: !0,
                        autoSkipPadding: 0,
                        labelOffset: 0,
                        callback: function(t) {
                            return e.isArray(t) ? t : "" + t
                        }
                    }
                }, t.Scale = t.Element.extend({
                    beforeUpdate: function() {
                        e.callCallback(this.options.beforeUpdate, [this])
                    },
                    update: function(t, a, i) {
                        var n = this;
                        return n.beforeUpdate(), n.maxWidth = t, n.maxHeight = a, n.margins = e.extend({
                            left: 0,
                            right: 0,
                            top: 0,
                            bottom: 0
                        }, i), n.beforeSetDimensions(), n.setDimensions(), n.afterSetDimensions(), n.beforeDataLimits(), n.determineDataLimits(), n.afterDataLimits(), n.beforeBuildTicks(), n.buildTicks(), n.afterBuildTicks(), n.beforeTickToLabelConversion(), n.convertTicksToLabels(), n.afterTickToLabelConversion(), n.beforeCalculateTickRotation(), n.calculateTickRotation(), n.afterCalculateTickRotation(), n.beforeFit(), n.fit(), n.afterFit(), n.afterUpdate(), n.minSize
                    },
                    afterUpdate: function() {
                        e.callCallback(this.options.afterUpdate, [this])
                    },
                    beforeSetDimensions: function() {
                        e.callCallback(this.options.beforeSetDimensions, [this])
                    },
                    setDimensions: function() {
                        var t = this;
                        t.isHorizontal() ? (t.width = t.maxWidth, t.left = 0, t.right = t.width) : (t.height = t.maxHeight, t.top = 0, t.bottom = t.height), t.paddingLeft = 0, t.paddingTop = 0, t.paddingRight = 0, t.paddingBottom = 0
                    },
                    afterSetDimensions: function() {
                        e.callCallback(this.options.afterSetDimensions, [this])
                    },
                    beforeDataLimits: function() {
                        e.callCallback(this.options.beforeDataLimits, [this])
                    },
                    determineDataLimits: e.noop,
                    afterDataLimits: function() {
                        e.callCallback(this.options.afterDataLimits, [this])
                    },
                    beforeBuildTicks: function() {
                        e.callCallback(this.options.beforeBuildTicks, [this])
                    },
                    buildTicks: e.noop,
                    afterBuildTicks: function() {
                        e.callCallback(this.options.afterBuildTicks, [this])
                    },
                    beforeTickToLabelConversion: function() {
                        e.callCallback(this.options.beforeTickToLabelConversion, [this])
                    },
                    convertTicksToLabels: function() {
                        var t = this;
                        t.ticks = t.ticks.map(function(e, a, i) {
                            return t.options.ticks.userCallback ? t.options.ticks.userCallback(e, a, i) : t.options.ticks.callback(e, a, i)
                        }, t)
                    },
                    afterTickToLabelConversion: function() {
                        e.callCallback(this.options.afterTickToLabelConversion, [this])
                    },
                    beforeCalculateTickRotation: function() {
                        e.callCallback(this.options.beforeCalculateTickRotation, [this])
                    },
                    calculateTickRotation: function() {
                        var a = this,
                            i = a.ctx,
                            n = t.defaults.global,
                            o = a.options.ticks,
                            r = e.getValueOrDefault(o.fontSize, n.defaultFontSize),
                            l = e.getValueOrDefault(o.fontStyle, n.defaultFontStyle),
                            s = e.getValueOrDefault(o.fontFamily, n.defaultFontFamily),
                            d = e.fontString(r, l, s);
                        i.font = d;
                        var u, c = i.measureText(a.ticks[0]).width,
                            h = i.measureText(a.ticks[a.ticks.length - 1]).width;
                        if (a.labelRotation = o.minRotation || 0, a.paddingRight = 0, a.paddingLeft = 0, a.options.display && a.isHorizontal()) {
                            a.paddingRight = h / 2 + 3, a.paddingLeft = c / 2 + 3, a.longestTextCache || (a.longestTextCache = {});
                            for (var f, g, p = e.longestText(i, d, a.ticks, a.longestTextCache), m = p, b = a.getPixelForTick(1) - a.getPixelForTick(0) - 6; m > b && a.labelRotation < o.maxRotation;) {
                                if (f = Math.cos(e.toRadians(a.labelRotation)), g = Math.sin(e.toRadians(a.labelRotation)), u = f * c, u + r / 2 > a.yLabelWidth && (a.paddingLeft = u + r / 2), a.paddingRight = r / 2, g * p > a.maxHeight) {
                                    a.labelRotation--;
                                    break
                                }
                                a.labelRotation++, m = f * p
                            }
                        }
                        a.margins && (a.paddingLeft = Math.max(a.paddingLeft - a.margins.left, 0), a.paddingRight = Math.max(a.paddingRight - a.margins.right, 0))
                    },
                    afterCalculateTickRotation: function() {
                        e.callCallback(this.options.afterCalculateTickRotation, [this])
                    },
                    beforeFit: function() {
                        e.callCallback(this.options.beforeFit, [this]);
                    },
                    fit: function() {
                        var a = this,
                            i = a.minSize = {
                                width: 0,
                                height: 0
                            },
                            n = a.options,
                            o = t.defaults.global,
                            r = n.ticks,
                            l = n.scaleLabel,
                            s = n.display,
                            d = a.isHorizontal(),
                            u = e.getValueOrDefault(r.fontSize, o.defaultFontSize),
                            c = e.getValueOrDefault(r.fontStyle, o.defaultFontStyle),
                            h = e.getValueOrDefault(r.fontFamily, o.defaultFontFamily),
                            f = e.fontString(u, c, h),
                            g = e.getValueOrDefault(l.fontSize, o.defaultFontSize),
                            p = n.gridLines.tickMarkLength;
                        if (d ? i.width = a.isFullWidth() ? a.maxWidth - a.margins.left - a.margins.right : a.maxWidth : i.width = s ? p : 0, d ? i.height = s ? p : 0 : i.height = a.maxHeight, l.display && s && (d ? i.height += 1.5 * g : i.width += 1.5 * g), r.display && s) {
                            a.longestTextCache || (a.longestTextCache = {});
                            var m = e.longestText(a.ctx, f, a.ticks, a.longestTextCache),
                                b = e.numberOfLabelLines(a.ticks),
                                v = .5 * u;
                            if (d) {
                                a.longestLabelWidth = m;
                                var x = Math.sin(e.toRadians(a.labelRotation)) * a.longestLabelWidth + u * b + v * b;
                                i.height = Math.min(a.maxHeight, i.height + x), a.ctx.font = f;
                                var y = a.ctx.measureText(a.ticks[0]).width,
                                    k = a.ctx.measureText(a.ticks[a.ticks.length - 1]).width,
                                    S = Math.cos(e.toRadians(a.labelRotation)),
                                    w = Math.sin(e.toRadians(a.labelRotation));
                                a.paddingLeft = 0 !== a.labelRotation ? S * y + 3 : y / 2 + 3, a.paddingRight = 0 !== a.labelRotation ? w * (u / 2) + 3 : k / 2 + 3
                            } else {
                                var C = a.maxWidth - i.width,
                                    M = r.mirror;
                                M ? m = 0 : m += a.options.ticks.padding, C > m ? i.width += m : i.width = a.maxWidth, a.paddingTop = u / 2, a.paddingBottom = u / 2
                            }
                        }
                        a.margins && (a.paddingLeft = Math.max(a.paddingLeft - a.margins.left, 0), a.paddingTop = Math.max(a.paddingTop - a.margins.top, 0), a.paddingRight = Math.max(a.paddingRight - a.margins.right, 0), a.paddingBottom = Math.max(a.paddingBottom - a.margins.bottom, 0)), a.width = i.width, a.height = i.height
                    },
                    afterFit: function() {
                        e.callCallback(this.options.afterFit, [this])
                    },
                    isHorizontal: function() {
                        return "top" === this.options.position || "bottom" === this.options.position
                    },
                    isFullWidth: function() {
                        return this.options.fullWidth
                    },
                    getRightValue: function(t) {
                        return null === t || "undefined" == typeof t ? NaN : "number" == typeof t && isNaN(t) ? NaN : "object" == typeof t ? t instanceof Date || t.isValid ? t : this.getRightValue(this.isHorizontal() ? t.x : t.y) : t
                    },
                    getLabelForIndex: e.noop,
                    getPixelForValue: e.noop,
                    getValueForPixel: e.noop,
                    getPixelForTick: function(t, e) {
                        var a = this;
                        if (a.isHorizontal()) {
                            var i = a.width - (a.paddingLeft + a.paddingRight),
                                n = i / Math.max(a.ticks.length - (a.options.gridLines.offsetGridLines ? 0 : 1), 1),
                                o = n * t + a.paddingLeft;
                            e && (o += n / 2);
                            var r = a.left + Math.round(o);
                            return r += a.isFullWidth() ? a.margins.left : 0
                        }
                        var l = a.height - (a.paddingTop + a.paddingBottom);
                        return a.top + t * (l / (a.ticks.length - 1))
                    },
                    getPixelForDecimal: function(t) {
                        var e = this;
                        if (e.isHorizontal()) {
                            var a = e.width - (e.paddingLeft + e.paddingRight),
                                i = a * t + e.paddingLeft,
                                n = e.left + Math.round(i);
                            return n += e.isFullWidth() ? e.margins.left : 0
                        }
                        return e.top + t * e.height
                    },
                    getBasePixel: function() {
                        var t = this,
                            e = t.min,
                            a = t.max;
                        return t.getPixelForValue(t.beginAtZero ? 0 : 0 > e && 0 > a ? a : e > 0 && a > 0 ? e : 0)
                    },
                    draw: function(a) {
                        var i = this,
                            n = i.options;
                        if (n.display) {
                            var o, r, l = i.ctx,
                                s = t.defaults.global,
                                d = n.ticks,
                                u = n.gridLines,
                                c = n.scaleLabel,
                                h = 0 !== i.labelRotation,
                                f = d.autoSkip,
                                g = i.isHorizontal();
                            d.maxTicksLimit && (r = d.maxTicksLimit);
                            var p = e.getValueOrDefault(d.fontColor, s.defaultFontColor),
                                m = e.getValueOrDefault(d.fontSize, s.defaultFontSize),
                                b = e.getValueOrDefault(d.fontStyle, s.defaultFontStyle),
                                v = e.getValueOrDefault(d.fontFamily, s.defaultFontFamily),
                                x = e.fontString(m, b, v),
                                y = u.tickMarkLength,
                                k = e.getValueOrDefault(c.fontColor, s.defaultFontColor),
                                S = e.getValueOrDefault(c.fontSize, s.defaultFontSize),
                                w = e.getValueOrDefault(c.fontStyle, s.defaultFontStyle),
                                C = e.getValueOrDefault(c.fontFamily, s.defaultFontFamily),
                                M = e.fontString(S, w, C),
                                D = e.toRadians(i.labelRotation),
                                A = Math.cos(D),
                                I = i.longestLabelWidth * A;
                            l.fillStyle = p;
                            var T = [];
                            if (g) {
                                if (o = !1, h && (I /= 2), (I + d.autoSkipPadding) * i.ticks.length > i.width - (i.paddingLeft + i.paddingRight) && (o = 1 + Math.floor((I + d.autoSkipPadding) * i.ticks.length / (i.width - (i.paddingLeft + i.paddingRight)))), r && i.ticks.length > r)
                                    for (; !o || i.ticks.length / (o || 1) > r;) o || (o = 1), o += 1;
                                f || (o = !1)
                            }
                            var F = "right" === n.position ? i.left : i.right - y,
                                P = "right" === n.position ? i.left + y : i.right,
                                R = "bottom" === n.position ? i.top : i.bottom - y,
                                _ = "bottom" === n.position ? i.top + y : i.bottom;
                            if (e.each(i.ticks, function(t, r) {
                                    if (void 0 !== t && null !== t) {
                                        var l = i.ticks.length === r + 1,
                                            s = o > 1 && r % o > 0 || r % o === 0 && r + o >= i.ticks.length;
                                        if ((!s || l) && void 0 !== t && null !== t) {
                                            var c, f;
                                            r === ("undefined" != typeof i.zeroLineIndex ? i.zeroLineIndex : 0) ? (c = u.zeroLineWidth, f = u.zeroLineColor) : (c = e.getValueAtIndexOrDefault(u.lineWidth, r), f = e.getValueAtIndexOrDefault(u.color, r));
                                            var p, m, b, v, x, k, S, w, C, M, A, I = "middle";
                                            if (g) {
                                                h || (I = "top" === n.position ? "bottom" : "top"), A = h ? "right" : "center";
                                                var V = i.getPixelForTick(r) + e.aliasPixel(c);
                                                C = i.getPixelForTick(r, u.offsetGridLines) + d.labelOffset, M = h ? i.top + 12 : "top" === n.position ? i.bottom - y : i.top + y, p = b = x = S = V, m = R, v = _, k = a.top, w = a.bottom
                                            } else {
                                                "left" === n.position ? d.mirror ? (C = i.right + d.padding, A = "left") : (C = i.right - d.padding, A = "right") : d.mirror ? (C = i.left - d.padding, A = "right") : (C = i.left + d.padding, A = "left");
                                                var L = i.getPixelForTick(r);
                                                L += e.aliasPixel(c), M = i.getPixelForTick(r, u.offsetGridLines), p = F, b = P, x = a.left, S = a.right, m = v = k = w = L
                                            }
                                            T.push({
                                                tx1: p,
                                                ty1: m,
                                                tx2: b,
                                                ty2: v,
                                                x1: x,
                                                y1: k,
                                                x2: S,
                                                y2: w,
                                                labelX: C,
                                                labelY: M,
                                                glWidth: c,
                                                glColor: f,
                                                rotation: -1 * D,
                                                label: t,
                                                textBaseline: I,
                                                textAlign: A
                                            })
                                        }
                                    }
                                }), e.each(T, function(t) {
                                    if (u.display && (l.lineWidth = t.glWidth, l.strokeStyle = t.glColor, l.beginPath(), u.drawTicks && (l.moveTo(t.tx1, t.ty1), l.lineTo(t.tx2, t.ty2)), u.drawOnChartArea && (l.moveTo(t.x1, t.y1), l.lineTo(t.x2, t.y2)), l.stroke()), d.display) {
                                        l.save(), l.translate(t.labelX, t.labelY), l.rotate(t.rotation), l.font = x, l.textBaseline = t.textBaseline, l.textAlign = t.textAlign;
                                        var a = t.label;
                                        if (e.isArray(a))
                                            for (var i = 0, n = 0; i < a.length; ++i) l.fillText("" + a[i], 0, n), n += 1.5 * m;
                                        else l.fillText(a, 0, 0);
                                        l.restore()
                                    }
                                }), c.display) {
                                var V, L, O = 0;
                                if (g) V = i.left + (i.right - i.left) / 2, L = "bottom" === n.position ? i.bottom - S / 2 : i.top + S / 2;
                                else {
                                    var B = "left" === n.position;
                                    V = B ? i.left + S / 2 : i.right - S / 2, L = i.top + (i.bottom - i.top) / 2, O = B ? -.5 * Math.PI : .5 * Math.PI
                                }
                                l.save(), l.translate(V, L), l.rotate(O), l.textAlign = "center", l.textBaseline = "middle", l.fillStyle = k, l.font = M, l.fillText(c.labelString, 0, 0), l.restore()
                            }
                            if (u.drawBorder) {
                                l.lineWidth = e.getValueAtIndexOrDefault(u.lineWidth, 0), l.strokeStyle = e.getValueAtIndexOrDefault(u.color, 0);
                                var W = i.left,
                                    z = i.right,
                                    N = i.top,
                                    H = i.bottom,
                                    E = e.aliasPixel(l.lineWidth);
                                g ? (N = H = "top" === n.position ? i.bottom : i.top, N += E, H += E) : (W = z = "left" === n.position ? i.right : i.left, W += E, z += E), l.beginPath(), l.moveTo(W, N), l.lineTo(z, H), l.stroke()
                            }
                        }
                    }
                })
            }
        }, {}],
        32: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                var e = t.helpers;
                t.scaleService = {
                    constructors: {},
                    defaults: {},
                    registerScaleType: function(t, a, i) {
                        this.constructors[t] = a, this.defaults[t] = e.clone(i)
                    },
                    getScaleConstructor: function(t) {
                        return this.constructors.hasOwnProperty(t) ? this.constructors[t] : void 0
                    },
                    getScaleDefaults: function(a) {
                        return this.defaults.hasOwnProperty(a) ? e.scaleMerge(t.defaults.scale, this.defaults[a]) : {}
                    },
                    updateScaleDefaults: function(t, a) {
                        var i = this.defaults;
                        i.hasOwnProperty(t) && (i[t] = e.extend(i[t], a))
                    },
                    addScalesToLayout: function(a) {
                        e.each(a.scales, function(e) {
                            t.layoutService.addBox(a, e)
                        })
                    }
                }
            }
        }, {}],
        33: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                var e = t.helpers;
                t.defaults.global.title = {
                    display: !1,
                    position: "top",
                    fullWidth: !0,
                    fontStyle: "bold",
                    padding: 10,
                    text: ""
                };
                var a = e.noop;
                t.Title = t.Element.extend({
                    initialize: function(a) {
                        var i = this;
                        e.extend(i, a), i.options = e.configMerge(t.defaults.global.title, a.options), i.legendHitBoxes = []
                    },
                    beforeUpdate: function() {
                        var a = this.chart.options;
                        a && a.title && (this.options = e.configMerge(t.defaults.global.title, a.title))
                    },
                    update: function(t, e, a) {
                        var i = this;
                        return i.beforeUpdate(), i.maxWidth = t, i.maxHeight = e, i.margins = a, i.beforeSetDimensions(), i.setDimensions(), i.afterSetDimensions(), i.beforeBuildLabels(), i.buildLabels(), i.afterBuildLabels(), i.beforeFit(), i.fit(), i.afterFit(), i.afterUpdate(), i.minSize
                    },
                    afterUpdate: a,
                    beforeSetDimensions: a,
                    setDimensions: function() {
                        var t = this;
                        t.isHorizontal() ? (t.width = t.maxWidth, t.left = 0, t.right = t.width) : (t.height = t.maxHeight, t.top = 0, t.bottom = t.height), t.paddingLeft = 0, t.paddingTop = 0, t.paddingRight = 0, t.paddingBottom = 0, t.minSize = {
                            width: 0,
                            height: 0
                        }
                    },
                    afterSetDimensions: a,
                    beforeBuildLabels: a,
                    buildLabels: a,
                    afterBuildLabels: a,
                    beforeFit: a,
                    fit: function() {
                        var a = this,
                            i = e.getValueOrDefault,
                            n = a.options,
                            o = t.defaults.global,
                            r = n.display,
                            l = i(n.fontSize, o.defaultFontSize),
                            s = a.minSize;
                        a.isHorizontal() ? (s.width = a.maxWidth, s.height = r ? l + 2 * n.padding : 0) : (s.width = r ? l + 2 * n.padding : 0, s.height = a.maxHeight), a.width = s.width, a.height = s.height
                    },
                    afterFit: a,
                    isHorizontal: function() {
                        var t = this.options.position;
                        return "top" === t || "bottom" === t
                    },
                    draw: function() {
                        var a = this,
                            i = a.ctx,
                            n = e.getValueOrDefault,
                            o = a.options,
                            r = t.defaults.global;
                        if (o.display) {
                            var l, s, d = n(o.fontSize, r.defaultFontSize),
                                u = n(o.fontStyle, r.defaultFontStyle),
                                c = n(o.fontFamily, r.defaultFontFamily),
                                h = e.fontString(d, u, c),
                                f = 0,
                                g = a.top,
                                p = a.left,
                                m = a.bottom,
                                b = a.right;
                            i.fillStyle = n(o.fontColor, r.defaultFontColor), i.font = h, a.isHorizontal() ? (l = p + (b - p) / 2, s = g + (m - g) / 2) : (l = "left" === o.position ? p + d / 2 : b - d / 2, s = g + (m - g) / 2, f = Math.PI * ("left" === o.position ? -.5 : .5)), i.save(), i.translate(l, s), i.rotate(f), i.textAlign = "center", i.textBaseline = "middle", i.fillText(o.text, 0, 0), i.restore()
                        }
                    }
                }), t.plugins.register({
                    beforeInit: function(e) {
                        var a = e.options,
                            i = a.title;
                        i && (e.titleBlock = new t.Title({
                            ctx: e.chart.ctx,
                            options: i,
                            chart: e
                        }), t.layoutService.addBox(e, e.titleBlock))
                    }
                })
            }
        }, {}],
        34: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                function e(t, e) {
                    return e && (n.isArray(e) ? Array.prototype.push.apply(t, e) : t.push(e)), t
                }

                function a(t) {
                    if (!t.length) return !1;
                    var e, a, i = [],
                        n = [];
                    for (e = 0, a = t.length; a > e; ++e) {
                        var o = t[e];
                        if (o && o.hasValue()) {
                            var r = o.tooltipPosition();
                            i.push(r.x), n.push(r.y)
                        }
                    }
                    var l = 0,
                        s = 0;
                    for (e = 0; e < i.length; ++e) i[e] && (l += i[e], s += n[e]);
                    return {
                        x: Math.round(l / i.length),
                        y: Math.round(s / i.length)
                    }
                }

                function i(t) {
                    var e = t._xScale,
                        a = t._yScale || t._scale,
                        i = t._index,
                        n = t._datasetIndex;
                    return {
                        xLabel: e ? e.getLabelForIndex(i, n) : "",
                        yLabel: a ? a.getLabelForIndex(i, n) : "",
                        index: i,
                        datasetIndex: n
                    }
                }
                var n = t.helpers;
                t.defaults.global.tooltips = {
                    enabled: !0,
                    custom: null,
                    mode: "single",
                    backgroundColor: "rgba(0,0,0,0.8)",
                    titleFontStyle: "bold",
                    titleSpacing: 2,
                    titleMarginBottom: 6,
                    titleFontColor: "#fff",
                    titleAlign: "left",
                    bodySpacing: 2,
                    bodyFontColor: "#fff",
                    bodyAlign: "left",
                    footerFontStyle: "bold",
                    footerSpacing: 2,
                    footerMarginTop: 6,
                    footerFontColor: "#fff",
                    footerAlign: "left",
                    yPadding: 6,
                    xPadding: 6,
                    yAlign: "center",
                    xAlign: "center",
                    caretSize: 5,
                    cornerRadius: 6,
                    multiKeyBackground: "#fff",
                    callbacks: {
                        beforeTitle: n.noop,
                        title: function(t, e) {
                            var a = "",
                                i = e.labels,
                                n = i ? i.length : 0;
                            if (t.length > 0) {
                                var o = t[0];
                                o.xLabel ? a = o.xLabel : n > 0 && o.index < n && (a = i[o.index])
                            }
                            return a
                        },
                        afterTitle: n.noop,
                        beforeBody: n.noop,
                        beforeLabel: n.noop,
                        label: function(t, e) {
                            var a = e.datasets[t.datasetIndex].label || "";
                            return a + ": " + t.yLabel
                        },
                        labelColor: function(t, e) {
                            var a = e.getDatasetMeta(t.datasetIndex),
                                i = a.data[t.index],
                                n = i._view;
                            return {
                                borderColor: n.borderColor,
                                backgroundColor: n.backgroundColor
                            }
                        },
                        afterLabel: n.noop,
                        afterBody: n.noop,
                        beforeFooter: n.noop,
                        footer: n.noop,
                        afterFooter: n.noop
                    }
                }, t.Tooltip = t.Element.extend({
                    initialize: function() {
                        var e = this,
                            a = t.defaults.global,
                            i = e._options,
                            o = n.getValueOrDefault;
                        n.extend(e, {
                            _model: {
                                xPadding: i.xPadding,
                                yPadding: i.yPadding,
                                xAlign: i.xAlign,
                                yAlign: i.yAlign,
                                bodyFontColor: i.bodyFontColor,
                                _bodyFontFamily: o(i.bodyFontFamily, a.defaultFontFamily),
                                _bodyFontStyle: o(i.bodyFontStyle, a.defaultFontStyle),
                                _bodyAlign: i.bodyAlign,
                                bodyFontSize: o(i.bodyFontSize, a.defaultFontSize),
                                bodySpacing: i.bodySpacing,
                                titleFontColor: i.titleFontColor,
                                _titleFontFamily: o(i.titleFontFamily, a.defaultFontFamily),
                                _titleFontStyle: o(i.titleFontStyle, a.defaultFontStyle),
                                titleFontSize: o(i.titleFontSize, a.defaultFontSize),
                                _titleAlign: i.titleAlign,
                                titleSpacing: i.titleSpacing,
                                titleMarginBottom: i.titleMarginBottom,
                                footerFontColor: i.footerFontColor,
                                _footerFontFamily: o(i.footerFontFamily, a.defaultFontFamily),
                                _footerFontStyle: o(i.footerFontStyle, a.defaultFontStyle),
                                footerFontSize: o(i.footerFontSize, a.defaultFontSize),
                                _footerAlign: i.footerAlign,
                                footerSpacing: i.footerSpacing,
                                footerMarginTop: i.footerMarginTop,
                                caretSize: i.caretSize,
                                cornerRadius: i.cornerRadius,
                                backgroundColor: i.backgroundColor,
                                opacity: 0,
                                legendColorBackground: i.multiKeyBackground
                            }
                        })
                    },
                    getTitle: function() {
                        var t = this,
                            a = t._options,
                            i = a.callbacks,
                            n = i.beforeTitle.apply(t, arguments),
                            o = i.title.apply(t, arguments),
                            r = i.afterTitle.apply(t, arguments),
                            l = [];
                        return l = e(l, n), l = e(l, o), l = e(l, r)
                    },
                    getBeforeBody: function() {
                        var t = this._options.callbacks.beforeBody.apply(this, arguments);
                        return n.isArray(t) ? t : void 0 !== t ? [t] : []
                    },
                    getBody: function(t, a) {
                        var i = this,
                            o = i._options.callbacks,
                            r = [];
                        return n.each(t, function(t) {
                            var n = {
                                before: [],
                                lines: [],
                                after: []
                            };
                            e(n.before, o.beforeLabel.call(i, t, a)), e(n.lines, o.label.call(i, t, a)), e(n.after, o.afterLabel.call(i, t, a)), r.push(n)
                        }), r
                    },
                    getAfterBody: function() {
                        var t = this._options.callbacks.afterBody.apply(this, arguments);
                        return n.isArray(t) ? t : void 0 !== t ? [t] : []
                    },
                    getFooter: function() {
                        var t = this,
                            a = t._options.callbacks,
                            i = a.beforeFooter.apply(t, arguments),
                            n = a.footer.apply(t, arguments),
                            o = a.afterFooter.apply(t, arguments),
                            r = [];
                        return r = e(r, i), r = e(r, n), r = e(r, o)
                    },
                    update: function(t) {
                        var e, o, r = this,
                            l = r._options,
                            s = r._model,
                            d = r._active,
                            u = r._data,
                            c = r._chartInstance;
                        if (d.length) {
                            s.opacity = 1;
                            var h = [],
                                f = a(d),
                                g = [];
                            for (e = 0, o = d.length; o > e; ++e) g.push(i(d[e]));
                            l.itemSort && (g = g.sort(l.itemSort)), d.length > 1 && n.each(g, function(t) {
                                h.push(l.callbacks.labelColor.call(r, t, c))
                            }), n.extend(s, {
                                title: r.getTitle(g, u),
                                beforeBody: r.getBeforeBody(g, u),
                                body: r.getBody(g, u),
                                afterBody: r.getAfterBody(g, u),
                                footer: r.getFooter(g, u),
                                x: Math.round(f.x),
                                y: Math.round(f.y),
                                caretPadding: n.getValueOrDefault(f.padding, 2),
                                labelColors: h
                            });
                            var p = r.getTooltipSize(s);
                            r.determineAlignment(p), n.extend(s, r.getBackgroundPoint(s, p))
                        } else r._model.opacity = 0;
                        return t && l.custom && l.custom.call(r, s), r
                    },
                    getTooltipSize: function(t) {
                        var e = this._chart.ctx,
                            a = {
                                height: 2 * t.yPadding,
                                width: 0
                            },
                            i = t.body,
                            o = i.reduce(function(t, e) {
                                return t + e.before.length + e.lines.length + e.after.length
                            }, 0);
                        o += t.beforeBody.length + t.afterBody.length;
                        var r = t.title.length,
                            l = t.footer.length,
                            s = t.titleFontSize,
                            d = t.bodyFontSize,
                            u = t.footerFontSize;
                        a.height += r * s, a.height += (r - 1) * t.titleSpacing, a.height += r ? t.titleMarginBottom : 0, a.height += o * d, a.height += o ? (o - 1) * t.bodySpacing : 0, a.height += l ? t.footerMarginTop : 0, a.height += l * u, a.height += l ? (l - 1) * t.footerSpacing : 0;
                        var c = 0,
                            h = function(t) {
                                a.width = Math.max(a.width, e.measureText(t).width + c)
                            };
                        return e.font = n.fontString(s, t._titleFontStyle, t._titleFontFamily), n.each(t.title, h), e.font = n.fontString(d, t._bodyFontStyle, t._bodyFontFamily), n.each(t.beforeBody.concat(t.afterBody), h), c = i.length > 1 ? d + 2 : 0, n.each(i, function(t) {
                            n.each(t.before, h), n.each(t.lines, h), n.each(t.after, h)
                        }), c = 0, e.font = n.fontString(u, t._footerFontStyle, t._footerFontFamily), n.each(t.footer, h), a.width += 2 * t.xPadding, a
                    },
                    determineAlignment: function(t) {
                        var e = this,
                            a = e._model,
                            i = e._chart,
                            n = e._chartInstance.chartArea;
                        a.y < t.height ? a.yAlign = "top" : a.y > i.height - t.height && (a.yAlign = "bottom");
                        var o, r, l, s, d, u = (n.left + n.right) / 2,
                            c = (n.top + n.bottom) / 2;
                        "center" === a.yAlign ? (o = function(t) {
                            return u >= t
                        }, r = function(t) {
                            return t > u
                        }) : (o = function(e) {
                            return e <= t.width / 2
                        }, r = function(e) {
                            return e >= i.width - t.width / 2
                        }), l = function(e) {
                            return e + t.width > i.width
                        }, s = function(e) {
                            return e - t.width < 0
                        }, d = function(t) {
                            return c >= t ? "top" : "bottom"
                        }, o(a.x) ? (a.xAlign = "left", l(a.x) && (a.xAlign = "center", a.yAlign = d(a.y))) : r(a.x) && (a.xAlign = "right", s(a.x) && (a.xAlign = "center", a.yAlign = d(a.y)))
                    },
                    getBackgroundPoint: function(t, e) {
                        var a = {
                                x: t.x,
                                y: t.y
                            },
                            i = t.caretSize,
                            n = t.caretPadding,
                            o = t.cornerRadius,
                            r = t.xAlign,
                            l = t.yAlign,
                            s = i + n,
                            d = o + n;
                        return "right" === r ? a.x -= e.width : "center" === r && (a.x -= e.width / 2), "top" === l ? a.y += s : "bottom" === l ? a.y -= e.height + s : a.y -= e.height / 2, "center" === l ? "left" === r ? a.x += s : "right" === r && (a.x -= s) : "left" === r ? a.x -= d : "right" === r && (a.x += d), a
                    },
                    drawCaret: function(t, e, a) {
                        var i, o, r, l, s, d, u = this._view,
                            c = this._chart.ctx,
                            h = u.caretSize,
                            f = u.cornerRadius,
                            g = u.xAlign,
                            p = u.yAlign,
                            m = t.x,
                            b = t.y,
                            v = e.width,
                            x = e.height;
                        "center" === p ? ("left" === g ? (i = m, o = i - h, r = i) : (i = m + v, o = i + h, r = i), s = b + x / 2, l = s - h, d = s + h) : ("left" === g ? (i = m + f, o = i + h, r = o + h) : "right" === g ? (i = m + v - f, o = i - h, r = o - h) : (o = m + v / 2, i = o - h, r = o + h), "top" === p ? (l = b, s = l - h, d = l) : (l = b + x, s = l + h, d = l));
                        var y = n.color(u.backgroundColor);
                        c.fillStyle = y.alpha(a * y.alpha()).rgbString(), c.beginPath(), c.moveTo(i, l), c.lineTo(o, s), c.lineTo(r, d), c.closePath(), c.fill()
                    },
                    drawTitle: function(t, e, a, i) {
                        var o = e.title;
                        if (o.length) {
                            a.textAlign = e._titleAlign, a.textBaseline = "top";
                            var r = e.titleFontSize,
                                l = e.titleSpacing,
                                s = n.color(e.titleFontColor);
                            a.fillStyle = s.alpha(i * s.alpha()).rgbString(), a.font = n.fontString(r, e._titleFontStyle, e._titleFontFamily);
                            var d, u;
                            for (d = 0, u = o.length; u > d; ++d) a.fillText(o[d], t.x, t.y), t.y += r + l, d + 1 === o.length && (t.y += e.titleMarginBottom - l)
                        }
                    },
                    drawBody: function(t, e, a, i) {
                        var o = e.bodyFontSize,
                            r = e.bodySpacing,
                            l = e.body;
                        a.textAlign = e._bodyAlign, a.textBaseline = "top";
                        var s = n.color(e.bodyFontColor),
                            d = s.alpha(i * s.alpha()).rgbString();
                        a.fillStyle = d, a.font = n.fontString(o, e._bodyFontStyle, e._bodyFontFamily);
                        var u = 0,
                            c = function(e) {
                                a.fillText(e, t.x + u, t.y), t.y += o + r
                            };
                        n.each(e.beforeBody, c);
                        var h = l.length > 1;
                        u = h ? o + 2 : 0, n.each(l, function(r, l) {
                            n.each(r.before, c), n.each(r.lines, function(r) {
                                h && (a.fillStyle = n.color(e.legendColorBackground).alpha(i).rgbaString(), a.fillRect(t.x, t.y, o, o), a.strokeStyle = n.color(e.labelColors[l].borderColor).alpha(i).rgbaString(), a.strokeRect(t.x, t.y, o, o), a.fillStyle = n.color(e.labelColors[l].backgroundColor).alpha(i).rgbaString(), a.fillRect(t.x + 1, t.y + 1, o - 2, o - 2), a.fillStyle = d), c(r)
                            }), n.each(r.after, c)
                        }), u = 0, n.each(e.afterBody, c), t.y -= r
                    },
                    drawFooter: function(t, e, a, i) {
                        var o = e.footer;
                        if (o.length) {
                            t.y += e.footerMarginTop, a.textAlign = e._footerAlign, a.textBaseline = "top";
                            var r = n.color(e.footerFontColor);
                            a.fillStyle = r.alpha(i * r.alpha()).rgbString(), a.font = n.fontString(e.footerFontSize, e._footerFontStyle, e._footerFontFamily), n.each(o, function(i) {
                                a.fillText(i, t.x, t.y), t.y += e.footerFontSize + e.footerSpacing
                            })
                        }
                    },
                    draw: function() {
                        var t = this._chart.ctx,
                            e = this._view;
                        if (0 !== e.opacity) {
                            var a = this.getTooltipSize(e),
                                i = {
                                    x: e.x,
                                    y: e.y
                                },
                                o = Math.abs(e.opacity < .001) ? 0 : e.opacity;
                            if (this._options.enabled) {
                                var r = n.color(e.backgroundColor);
                                t.fillStyle = r.alpha(o * r.alpha()).rgbString(), n.drawRoundedRectangle(t, i.x, i.y, a.width, a.height, e.cornerRadius), t.fill(), this.drawCaret(i, a, o), i.x += e.xPadding, i.y += e.yPadding, this.drawTitle(i, e, t, o), this.drawBody(i, e, t, o), this.drawFooter(i, e, t, o)
                            }
                        }
                    }
                })
            }
        }, {}],
        35: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                var e = t.helpers,
                    a = t.defaults.global;
                a.elements.arc = {
                    backgroundColor: a.defaultColor,
                    borderColor: "#fff",
                    borderWidth: 2
                }, t.elements.Arc = t.Element.extend({
                    inLabelRange: function(t) {
                        var e = this._view;
                        return e ? Math.pow(t - e.x, 2) < Math.pow(e.radius + e.hoverRadius, 2) : !1
                    },
                    inRange: function(t, a) {
                        var i = this._view;
                        if (i) {
                            for (var n = e.getAngleFromPoint(i, {
                                    x: t,
                                    y: a
                                }), o = n.angle, r = n.distance, l = i.startAngle, s = i.endAngle; l > s;) s += 2 * Math.PI;
                            for (; o > s;) o -= 2 * Math.PI;
                            for (; l > o;) o += 2 * Math.PI;
                            var d = o >= l && s >= o,
                                u = r >= i.innerRadius && r <= i.outerRadius;
                            return d && u
                        }
                        return !1
                    },
                    tooltipPosition: function() {
                        var t = this._view,
                            e = t.startAngle + (t.endAngle - t.startAngle) / 2,
                            a = (t.outerRadius - t.innerRadius) / 2 + t.innerRadius;
                        return {
                            x: t.x + Math.cos(e) * a,
                            y: t.y + Math.sin(e) * a
                        }
                    },
                    draw: function() {
                        var t = this._chart.ctx,
                            e = this._view,
                            a = e.startAngle,
                            i = e.endAngle;
                        t.beginPath(), t.arc(e.x, e.y, e.outerRadius, a, i), t.arc(e.x, e.y, e.innerRadius, i, a, !0), t.closePath(), t.strokeStyle = e.borderColor, t.lineWidth = e.borderWidth, t.fillStyle = e.backgroundColor, t.fill(), t.lineJoin = "bevel", e.borderWidth && t.stroke()
                    }
                })
            }
        }, {}],
        36: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                var e = t.helpers,
                    a = t.defaults.global;
                t.defaults.global.elements.line = {
                    tension: .4,
                    backgroundColor: a.defaultColor,
                    borderWidth: 3,
                    borderColor: a.defaultColor,
                    borderCapStyle: "butt",
                    borderDash: [],
                    borderDashOffset: 0,
                    borderJoinStyle: "miter",
                    capBezierPoints: !0,
                    fill: !0
                }, t.elements.Line = t.Element.extend({
                    draw: function() {
                        function t(t, e) {
                            var a = e._view;
                            e._view.steppedLine === !0 ? (s.lineTo(e._view.x, t._view.y), s.lineTo(e._view.x, e._view.y)) : 0 === e._view.tension ? s.lineTo(a.x, a.y) : s.bezierCurveTo(t._view.controlPointNextX, t._view.controlPointNextY, a.controlPointPreviousX, a.controlPointPreviousY, a.x, a.y)
                        }
                        var i = this,
                            n = i._view,
                            o = n.spanGaps,
                            r = n.scaleZero,
                            l = i._loop,
                            s = i._chart.ctx;
                        s.save();
                        var d = i._children.slice(),
                            u = -1;
                        l && d.length && d.push(d[0]);
                        var c, h, f, g;
                        if (d.length && n.fill) {
                            for (s.beginPath(), c = 0; c < d.length; ++c) h = d[c], f = e.previousItem(d, c), g = h._view, 0 === c ? (l ? s.moveTo(r.x, r.y) : s.moveTo(g.x, r), g.skip || (u = c, s.lineTo(g.x, g.y))) : (f = -1 === u ? f : d[u], g.skip ? o || u !== c - 1 || (l ? s.lineTo(r.x, r.y) : s.lineTo(f._view.x, r)) : (u !== c - 1 ? o && -1 !== u ? t(f, h) : l ? s.lineTo(g.x, g.y) : (s.lineTo(g.x, r), s.lineTo(g.x, g.y)) : t(f, h), u = c));
                            l || s.lineTo(d[u]._view.x, r), s.fillStyle = n.backgroundColor || a.defaultColor, s.closePath(), s.fill()
                        }
                        var p = a.elements.line;
                        for (s.lineCap = n.borderCapStyle || p.borderCapStyle, s.setLineDash && s.setLineDash(n.borderDash || p.borderDash), s.lineDashOffset = n.borderDashOffset || p.borderDashOffset, s.lineJoin = n.borderJoinStyle || p.borderJoinStyle, s.lineWidth = n.borderWidth || p.borderWidth, s.strokeStyle = n.borderColor || a.defaultColor, s.beginPath(), u = -1, c = 0; c < d.length; ++c) h = d[c], f = e.previousItem(d, c), g = h._view, 0 === c ? g.skip || (s.moveTo(g.x, g.y), u = c) : (f = -1 === u ? f : d[u], g.skip || (u !== c - 1 && !o || -1 === u ? s.moveTo(g.x, g.y) : t(f, h), u = c));
                        s.stroke(), s.restore()
                    }
                })
            }
        }, {}],
        37: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                var e = t.helpers,
                    a = t.defaults.global,
                    i = a.defaultColor;
                a.elements.point = {
                    radius: 3,
                    pointStyle: "circle",
                    backgroundColor: i,
                    borderWidth: 1,
                    borderColor: i,
                    hitRadius: 1,
                    hoverRadius: 4,
                    hoverBorderWidth: 1
                }, t.elements.Point = t.Element.extend({
                    inRange: function(t, e) {
                        var a = this._view;
                        return a ? Math.pow(t - a.x, 2) + Math.pow(e - a.y, 2) < Math.pow(a.hitRadius + a.radius, 2) : !1
                    },
                    inLabelRange: function(t) {
                        var e = this._view;
                        return e ? Math.pow(t - e.x, 2) < Math.pow(e.radius + e.hitRadius, 2) : !1
                    },
                    tooltipPosition: function() {
                        var t = this._view;
                        return {
                            x: t.x,
                            y: t.y,
                            padding: t.radius + t.borderWidth
                        }
                    },
                    draw: function() {
                        var n = this._view,
                            o = this._chart.ctx,
                            r = n.pointStyle,
                            l = n.radius,
                            s = n.x,
                            d = n.y;
                        n.skip || (o.strokeStyle = n.borderColor || i, o.lineWidth = e.getValueOrDefault(n.borderWidth, a.elements.point.borderWidth), o.fillStyle = n.backgroundColor || i, t.canvasHelpers.drawPoint(o, r, l, s, d))
                    }
                })
            }
        }, {}],
        38: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                var e = t.defaults.global;
                e.elements.rectangle = {
                    backgroundColor: e.defaultColor,
                    borderWidth: 0,
                    borderColor: e.defaultColor,
                    borderSkipped: "bottom"
                }, t.elements.Rectangle = t.Element.extend({
                    draw: function() {
                        function t(t) {
                            return s[(u + t) % 4]
                        }
                        var e = this._chart.ctx,
                            a = this._view,
                            i = a.width / 2,
                            n = a.x - i,
                            o = a.x + i,
                            r = a.base - (a.base - a.y),
                            l = a.borderWidth / 2;
                        a.borderWidth && (n += l, o -= l, r += l), e.beginPath(), e.fillStyle = a.backgroundColor, e.strokeStyle = a.borderColor, e.lineWidth = a.borderWidth;
                        var s = [
                                [n, a.base],
                                [n, r],
                                [o, r],
                                [o, a.base]
                            ],
                            d = ["bottom", "left", "top", "right"],
                            u = d.indexOf(a.borderSkipped, 0); - 1 === u && (u = 0), e.moveTo.apply(e, t(0));
                        for (var c = 1; 4 > c; c++) e.lineTo.apply(e, t(c));
                        e.fill(), a.borderWidth && e.stroke()
                    },
                    height: function() {
                        var t = this._view;
                        return t.base - t.y
                    },
                    inRange: function(t, e) {
                        var a = this._view;
                        return a ? a.y < a.base ? t >= a.x - a.width / 2 && t <= a.x + a.width / 2 && e >= a.y && e <= a.base : t >= a.x - a.width / 2 && t <= a.x + a.width / 2 && e >= a.base && e <= a.y : !1
                    },
                    inLabelRange: function(t) {
                        var e = this._view;
                        return e ? t >= e.x - e.width / 2 && t <= e.x + e.width / 2 : !1
                    },
                    tooltipPosition: function() {
                        var t = this._view;
                        return {
                            x: t.x,
                            y: t.y
                        }
                    }
                })
            }
        }, {}],
        39: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                var e = t.helpers,
                    a = {
                        position: "bottom"
                    },
                    i = t.Scale.extend({
                        getLabels: function() {
                            var t = this.chart.data;
                            return (this.isHorizontal() ? t.xLabels : t.yLabels) || t.labels
                        },
                        determineDataLimits: function() {
                            var t = this,
                                a = t.getLabels();
                            t.minIndex = 0, t.maxIndex = a.length - 1;
                            var i;
                            void 0 !== t.options.ticks.min && (i = e.indexOf(a, t.options.ticks.min), t.minIndex = -1 !== i ? i : t.minIndex), void 0 !== t.options.ticks.max && (i = e.indexOf(a, t.options.ticks.max), t.maxIndex = -1 !== i ? i : t.maxIndex), t.min = a[t.minIndex], t.max = a[t.maxIndex]
                        },
                        buildTicks: function() {
                            var t = this,
                                e = t.getLabels();
                            t.ticks = 0 === t.minIndex && t.maxIndex === e.length - 1 ? e : e.slice(t.minIndex, t.maxIndex + 1)
                        },
                        getLabelForIndex: function(t) {
                            return this.ticks[t]
                        },
                        getPixelForValue: function(t, e, a, i) {
                            var n = this,
                                o = Math.max(n.maxIndex + 1 - n.minIndex - (n.options.gridLines.offsetGridLines ? 0 : 1), 1);
                            if (void 0 !== t) {
                                var r = n.getLabels(),
                                    l = r.indexOf(t);
                                e = -1 !== l ? l : e
                            }
                            if (n.isHorizontal()) {
                                var s = n.width - (n.paddingLeft + n.paddingRight),
                                    d = s / o,
                                    u = d * (e - n.minIndex) + n.paddingLeft;
                                return n.options.gridLines.offsetGridLines && i && (u += d / 2), n.left + Math.round(u)
                            }
                            var c = n.height - (n.paddingTop + n.paddingBottom),
                                h = c / o,
                                f = h * (e - n.minIndex) + n.paddingTop;
                            return n.options.gridLines.offsetGridLines && i && (f += h / 2), n.top + Math.round(f)
                        },
                        getPixelForTick: function(t, e) {
                            return this.getPixelForValue(this.ticks[t], t + this.minIndex, null, e)
                        },
                        getValueForPixel: function(t) {
                            var e, a = this,
                                i = Math.max(a.ticks.length - (a.options.gridLines.offsetGridLines ? 0 : 1), 1),
                                n = a.isHorizontal(),
                                o = n ? a.width - (a.paddingLeft + a.paddingRight) : a.height - (a.paddingTop + a.paddingBottom),
                                r = o / i;
                            return t -= n ? a.left : a.top, a.options.gridLines.offsetGridLines && (t -= r / 2), t -= n ? a.paddingLeft : a.paddingTop, e = 0 >= t ? 0 : Math.round(t / r)
                        },
                        getBasePixel: function() {
                            return this.bottom
                        }
                    });
                t.scaleService.registerScaleType("category", i, a)
            }
        }, {}],
        40: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                var e = t.helpers,
                    a = {
                        position: "left",
                        ticks: {
                            callback: function(t, a, i) {
                                var n = i.length > 3 ? i[2] - i[1] : i[1] - i[0];
                                Math.abs(n) > 1 && t !== Math.floor(t) && (n = t - Math.floor(t));
                                var o = e.log10(Math.abs(n)),
                                    r = "";
                                if (0 !== t) {
                                    var l = -1 * Math.floor(o);
                                    l = Math.max(Math.min(l, 20), 0), r = t.toFixed(l)
                                } else r = "0";
                                return r
                            }
                        }
                    },
                    i = t.LinearScaleBase.extend({
                        determineDataLimits: function() {
                            function t(t) {
                                return l ? t.xAxisID === a.id : t.yAxisID === a.id
                            }
                            var a = this,
                                i = a.options,
                                n = a.chart,
                                o = n.data,
                                r = o.datasets,
                                l = a.isHorizontal();
                            if (a.min = null, a.max = null, i.stacked) {
                                var s = {},
                                    d = !1,
                                    u = !1;
                                e.each(r, function(o, r) {
                                    var l = n.getDatasetMeta(r);
                                    void 0 === s[l.type] && (s[l.type] = {
                                        positiveValues: [],
                                        negativeValues: []
                                    });
                                    var c = s[l.type].positiveValues,
                                        h = s[l.type].negativeValues;
                                    n.isDatasetVisible(r) && t(l) && e.each(o.data, function(t, e) {
                                        var n = +a.getRightValue(t);
                                        isNaN(n) || l.data[e].hidden || (c[e] = c[e] || 0, h[e] = h[e] || 0, i.relativePoints ? c[e] = 100 : 0 > n ? (u = !0, h[e] += n) : (d = !0, c[e] += n))
                                    })
                                }), e.each(s, function(t) {
                                    var i = t.positiveValues.concat(t.negativeValues),
                                        n = e.min(i),
                                        o = e.max(i);
                                    a.min = null === a.min ? n : Math.min(a.min, n), a.max = null === a.max ? o : Math.max(a.max, o)
                                })
                            } else e.each(r, function(i, o) {
                                var r = n.getDatasetMeta(o);
                                n.isDatasetVisible(o) && t(r) && e.each(i.data, function(t, e) {
                                    var i = +a.getRightValue(t);
                                    isNaN(i) || r.data[e].hidden || (null === a.min ? a.min = i : i < a.min && (a.min = i), null === a.max ? a.max = i : i > a.max && (a.max = i))
                                })
                            });
                            this.handleTickRangeOptions()
                        },
                        getTickLimit: function() {
                            var a, i = this,
                                n = i.options.ticks;
                            if (i.isHorizontal()) a = Math.min(n.maxTicksLimit ? n.maxTicksLimit : 11, Math.ceil(i.width / 50));
                            else {
                                var o = e.getValueOrDefault(n.fontSize, t.defaults.global.defaultFontSize);
                                a = Math.min(n.maxTicksLimit ? n.maxTicksLimit : 11, Math.ceil(i.height / (2 * o)))
                            }
                            return a
                        },
                        handleDirectionalChanges: function() {
                            this.isHorizontal() || this.ticks.reverse()
                        },
                        getLabelForIndex: function(t, e) {
                            return +this.getRightValue(this.chart.data.datasets[e].data[t])
                        },
                        getPixelForValue: function(t) {
                            var e, a, i = this,
                                n = i.paddingLeft,
                                o = i.paddingBottom,
                                r = i.start,
                                l = +i.getRightValue(t),
                                s = i.end - r;
                            return i.isHorizontal() ? (a = i.width - (n + i.paddingRight), e = i.left + a / s * (l - r), Math.round(e + n)) : (a = i.height - (i.paddingTop + o), e = i.bottom - o - a / s * (l - r), Math.round(e))
                        },
                        getValueForPixel: function(t) {
                            var e = this,
                                a = e.isHorizontal(),
                                i = e.paddingLeft,
                                n = e.paddingBottom,
                                o = a ? e.width - (i + e.paddingRight) : e.height - (e.paddingTop + n),
                                r = (a ? t - e.left - i : e.bottom - n - t) / o;
                            return e.start + (e.end - e.start) * r
                        },
                        getPixelForTick: function(t) {
                            return this.getPixelForValue(this.ticksAsNumbers[t])
                        }
                    });
                t.scaleService.registerScaleType("linear", i, a)
            }
        }, {}],
        41: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                var e = t.helpers,
                    a = e.noop;
                t.LinearScaleBase = t.Scale.extend({
                    handleTickRangeOptions: function() {
                        var t = this,
                            a = t.options,
                            i = a.ticks;
                        if (i.beginAtZero) {
                            var n = e.sign(t.min),
                                o = e.sign(t.max);
                            0 > n && 0 > o ? t.max = 0 : n > 0 && o > 0 && (t.min = 0)
                        }
                        void 0 !== i.min ? t.min = i.min : void 0 !== i.suggestedMin && (t.min = Math.min(t.min, i.suggestedMin)), void 0 !== i.max ? t.max = i.max : void 0 !== i.suggestedMax && (t.max = Math.max(t.max, i.suggestedMax)), t.min === t.max && (t.max++, i.beginAtZero || t.min--)
                    },
                    getTickLimit: a,
                    handleDirectionalChanges: a,
                    buildTicks: function() {
                        var t = this,
                            a = t.options,
                            i = t.ticks = [],
                            n = a.ticks,
                            o = e.getValueOrDefault,
                            r = t.getTickLimit();
                        r = Math.max(2, r);
                        var l, s = n.fixedStepSize && n.fixedStepSize > 0 || n.stepSize && n.stepSize > 0;
                        if (s) l = o(n.fixedStepSize, n.stepSize);
                        else {
                            var d = e.niceNum(t.max - t.min, !1);
                            l = e.niceNum(d / (r - 1), !0)
                        }
                        var u = Math.floor(t.min / l) * l,
                            c = Math.ceil(t.max / l) * l,
                            h = (c - u) / l;
                        h = e.almostEquals(h, Math.round(h), l / 1e3) ? Math.round(h) : Math.ceil(h), i.push(void 0 !== n.min ? n.min : u);
                        for (var f = 1; h > f; ++f) i.push(u + f * l);
                        i.push(void 0 !== n.max ? n.max : c), t.handleDirectionalChanges(), t.max = e.max(i), t.min = e.min(i), n.reverse ? (i.reverse(), t.start = t.max, t.end = t.min) : (t.start = t.min, t.end = t.max)
                    },
                    convertTicksToLabels: function() {
                        var e = this;
                        e.ticksAsNumbers = e.ticks.slice(), e.zeroLineIndex = e.ticks.indexOf(0), t.Scale.prototype.convertTicksToLabels.call(e)
                    }
                })
            }
        }, {}],
        42: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                var e = t.helpers,
                    a = {
                        position: "left",
                        ticks: {
                            callback: function(t, a, i) {
                                var n = t / Math.pow(10, Math.floor(e.log10(t)));
                                return 1 === n || 2 === n || 5 === n || 0 === a || a === i.length - 1 ? t.toExponential() : ""
                            }
                        }
                    },
                    i = t.Scale.extend({
                        determineDataLimits: function() {
                            function t(t) {
                                return d ? t.xAxisID === a.id : t.yAxisID === a.id
                            }
                            var a = this,
                                i = a.options,
                                n = i.ticks,
                                o = a.chart,
                                r = o.data,
                                l = r.datasets,
                                s = e.getValueOrDefault,
                                d = a.isHorizontal();
                            if (a.min = null, a.max = null, i.stacked) {
                                var u = {};
                                e.each(l, function(n, r) {
                                    var l = o.getDatasetMeta(r);
                                    o.isDatasetVisible(r) && t(l) && (void 0 === u[l.type] && (u[l.type] = []), e.each(n.data, function(t, e) {
                                        var n = u[l.type],
                                            o = +a.getRightValue(t);
                                        isNaN(o) || l.data[e].hidden || (n[e] = n[e] || 0, i.relativePoints ? n[e] = 100 : n[e] += o)
                                    }))
                                }), e.each(u, function(t) {
                                    var i = e.min(t),
                                        n = e.max(t);
                                    a.min = null === a.min ? i : Math.min(a.min, i), a.max = null === a.max ? n : Math.max(a.max, n)
                                })
                            } else e.each(l, function(i, n) {
                                var r = o.getDatasetMeta(n);
                                o.isDatasetVisible(n) && t(r) && e.each(i.data, function(t, e) {
                                    var i = +a.getRightValue(t);
                                    isNaN(i) || r.data[e].hidden || (null === a.min ? a.min = i : i < a.min && (a.min = i), null === a.max ? a.max = i : i > a.max && (a.max = i))
                                })
                            });
                            a.min = s(n.min, a.min), a.max = s(n.max, a.max), a.min === a.max && (0 !== a.min && null !== a.min ? (a.min = Math.pow(10, Math.floor(e.log10(a.min)) - 1), a.max = Math.pow(10, Math.floor(e.log10(a.max)) + 1)) : (a.min = 1, a.max = 10))
                        },
                        buildTicks: function() {
                            for (var t = this, a = t.options, i = a.ticks, n = e.getValueOrDefault, o = t.ticks = [], r = n(i.min, Math.pow(10, Math.floor(e.log10(t.min)))); r < t.max;) {
                                o.push(r);
                                var l = Math.floor(e.log10(r)),
                                    s = Math.floor(r / Math.pow(10, l)) + 1;
                                10 === s && (s = 1, ++l), r = s * Math.pow(10, l)
                            }
                            var d = n(i.max, r);
                            o.push(d), t.isHorizontal() || o.reverse(), t.max = e.max(o), t.min = e.min(o), i.reverse ? (o.reverse(), t.start = t.max, t.end = t.min) : (t.start = t.min, t.end = t.max)
                        },
                        convertTicksToLabels: function() {
                            this.tickValues = this.ticks.slice(), t.Scale.prototype.convertTicksToLabels.call(this)
                        },
                        getLabelForIndex: function(t, e) {
                            return +this.getRightValue(this.chart.data.datasets[e].data[t])
                        },
                        getPixelForTick: function(t) {
                            return this.getPixelForValue(this.tickValues[t])
                        },
                        getPixelForValue: function(t) {
                            var a, i, n = this,
                                o = n.start,
                                r = +n.getRightValue(t),
                                l = e.log10(n.end) - e.log10(o),
                                s = n.paddingTop,
                                d = n.paddingBottom,
                                u = n.paddingLeft;
                            return n.isHorizontal() ? 0 === r ? i = n.left + u : (a = n.width - (u + n.paddingRight), i = n.left + a / l * (e.log10(r) - e.log10(o)), i += u) : 0 === r ? i = n.top + s : (a = n.height - (s + d), i = n.bottom - d - a / l * (e.log10(r) - e.log10(o))), i
                        },
                        getValueForPixel: function(t) {
                            var a, i, n = this,
                                o = e.log10(n.end) - e.log10(n.start);
                            return n.isHorizontal() ? (i = n.width - (n.paddingLeft + n.paddingRight), a = n.start * Math.pow(10, (t - n.left - n.paddingLeft) * o / i)) : (i = n.height - (n.paddingTop + n.paddingBottom), a = Math.pow(10, (n.bottom - n.paddingBottom - t) * o / i) / n.start), a
                        }
                    });
                t.scaleService.registerScaleType("logarithmic", i, a)
            }
        }, {}],
        43: [function(t, e, a) {
            "use strict";
            e.exports = function(t) {
                var e = t.helpers,
                    a = t.defaults.global,
                    i = {
                        display: !0,
                        animate: !0,
                        lineArc: !1,
                        position: "chartArea",
                        angleLines: {
                            display: !0,
                            color: "rgba(0, 0, 0, 0.1)",
                            lineWidth: 1
                        },
                        ticks: {
                            showLabelBackdrop: !0,
                            backdropColor: "rgba(255,255,255,0.75)",
                            backdropPaddingY: 2,
                            backdropPaddingX: 2
                        },
                        pointLabels: {
                            fontSize: 10,
                            callback: function(t) {
                                return t
                            }
                        }
                    },
                    n = t.LinearScaleBase.extend({
                        getValueCount: function() {
                            return this.chart.data.labels.length;
                        },
                        setDimensions: function() {
                            var t = this,
                                i = t.options,
                                n = i.ticks;
                            t.width = t.maxWidth, t.height = t.maxHeight, t.xCenter = Math.round(t.width / 2), t.yCenter = Math.round(t.height / 2);
                            var o = e.min([t.height, t.width]),
                                r = e.getValueOrDefault(n.fontSize, a.defaultFontSize);
                            t.drawingArea = i.display ? o / 2 - (r / 2 + n.backdropPaddingY) : o / 2
                        },
                        determineDataLimits: function() {
                            var t = this,
                                a = t.chart;
                            t.min = null, t.max = null, e.each(a.data.datasets, function(i, n) {
                                if (a.isDatasetVisible(n)) {
                                    var o = a.getDatasetMeta(n);
                                    e.each(i.data, function(e, a) {
                                        var i = +t.getRightValue(e);
                                        isNaN(i) || o.data[a].hidden || (null === t.min ? t.min = i : i < t.min && (t.min = i), null === t.max ? t.max = i : i > t.max && (t.max = i))
                                    })
                                }
                            }), t.handleTickRangeOptions()
                        },
                        getTickLimit: function() {
                            var t = this.options.ticks,
                                i = e.getValueOrDefault(t.fontSize, a.defaultFontSize);
                            return Math.min(t.maxTicksLimit ? t.maxTicksLimit : 11, Math.ceil(this.drawingArea / (1.5 * i)))
                        },
                        convertTicksToLabels: function() {
                            var e = this;
                            t.LinearScaleBase.prototype.convertTicksToLabels.call(e), e.pointLabels = e.chart.data.labels.map(e.options.pointLabels.callback, e)
                        },
                        getLabelForIndex: function(t, e) {
                            return +this.getRightValue(this.chart.data.datasets[e].data[t])
                        },
                        fit: function() {
                            var t, i, n, o, r, l, s, d, u, c, h, f, g = this.options.pointLabels,
                                p = e.getValueOrDefault(g.fontSize, a.defaultFontSize),
                                m = e.getValueOrDefault(g.fontStyle, a.defaultFontStyle),
                                b = e.getValueOrDefault(g.fontFamily, a.defaultFontFamily),
                                v = e.fontString(p, m, b),
                                x = e.min([this.height / 2 - p - 5, this.width / 2]),
                                y = this.width,
                                k = 0;
                            for (this.ctx.font = v, i = 0; i < this.getValueCount(); i++) {
                                t = this.getPointPosition(i, x), n = this.ctx.measureText(this.pointLabels[i] ? this.pointLabels[i] : "").width + 5;
                                var S = this.getIndexAngle(i) + Math.PI / 2,
                                    w = 360 * S / (2 * Math.PI) % 360;
                                0 === w || 180 === w ? (o = n / 2, t.x + o > y && (y = t.x + o, r = i), t.x - o < k && (k = t.x - o, s = i)) : 180 > w ? t.x + n > y && (y = t.x + n, r = i) : t.x - n < k && (k = t.x - n, s = i)
                            }
                            u = k, c = Math.ceil(y - this.width), l = this.getIndexAngle(r), d = this.getIndexAngle(s), h = c / Math.sin(l + Math.PI / 2), f = u / Math.sin(d + Math.PI / 2), h = e.isNumber(h) ? h : 0, f = e.isNumber(f) ? f : 0, this.drawingArea = Math.round(x - (f + h) / 2), this.setCenterPoint(f, h)
                        },
                        setCenterPoint: function(t, e) {
                            var a = this,
                                i = a.width - e - a.drawingArea,
                                n = t + a.drawingArea;
                            a.xCenter = Math.round((n + i) / 2 + a.left), a.yCenter = Math.round(a.height / 2 + a.top)
                        },
                        getIndexAngle: function(t) {
                            var e = 2 * Math.PI / this.getValueCount(),
                                a = this.chart.options && this.chart.options.startAngle ? this.chart.options.startAngle : 0,
                                i = a * Math.PI * 2 / 360;
                            return t * e - Math.PI / 2 + i
                        },
                        getDistanceFromCenterForValue: function(t) {
                            var e = this;
                            if (null === t) return 0;
                            var a = e.drawingArea / (e.max - e.min);
                            return e.options.reverse ? (e.max - t) * a : (t - e.min) * a
                        },
                        getPointPosition: function(t, e) {
                            var a = this,
                                i = a.getIndexAngle(t);
                            return {
                                x: Math.round(Math.cos(i) * e) + a.xCenter,
                                y: Math.round(Math.sin(i) * e) + a.yCenter
                            }
                        },
                        getPointPositionForValue: function(t, e) {
                            return this.getPointPosition(t, this.getDistanceFromCenterForValue(e))
                        },
                        getBasePosition: function() {
                            var t = this,
                                e = t.min,
                                a = t.max;
                            return t.getPointPositionForValue(0, t.beginAtZero ? 0 : 0 > e && 0 > a ? a : e > 0 && a > 0 ? e : 0)
                        },
                        draw: function() {
                            var t = this,
                                i = t.options,
                                n = i.gridLines,
                                o = i.ticks,
                                r = i.angleLines,
                                l = i.pointLabels,
                                s = e.getValueOrDefault;
                            if (i.display) {
                                var d = t.ctx,
                                    u = s(o.fontSize, a.defaultFontSize),
                                    c = s(o.fontStyle, a.defaultFontStyle),
                                    h = s(o.fontFamily, a.defaultFontFamily),
                                    f = e.fontString(u, c, h);
                                if (e.each(t.ticks, function(r, l) {
                                        if (l > 0 || i.reverse) {
                                            var c = t.getDistanceFromCenterForValue(t.ticksAsNumbers[l]),
                                                h = t.yCenter - c;
                                            if (n.display && 0 !== l)
                                                if (d.strokeStyle = e.getValueAtIndexOrDefault(n.color, l - 1), d.lineWidth = e.getValueAtIndexOrDefault(n.lineWidth, l - 1), i.lineArc) d.beginPath(), d.arc(t.xCenter, t.yCenter, c, 0, 2 * Math.PI), d.closePath(), d.stroke();
                                                else {
                                                    d.beginPath();
                                                    for (var g = 0; g < t.getValueCount(); g++) {
                                                        var p = t.getPointPosition(g, c);
                                                        0 === g ? d.moveTo(p.x, p.y) : d.lineTo(p.x, p.y)
                                                    }
                                                    d.closePath(), d.stroke()
                                                } if (o.display) {
                                                var m = s(o.fontColor, a.defaultFontColor);
                                                if (d.font = f, o.showLabelBackdrop) {
                                                    var b = d.measureText(r).width;
                                                    d.fillStyle = o.backdropColor, d.fillRect(t.xCenter - b / 2 - o.backdropPaddingX, h - u / 2 - o.backdropPaddingY, b + 2 * o.backdropPaddingX, u + 2 * o.backdropPaddingY)
                                                }
                                                d.textAlign = "center", d.textBaseline = "middle", d.fillStyle = m, d.fillText(r, t.xCenter, h)
                                            }
                                        }
                                    }), !i.lineArc) {
                                    d.lineWidth = r.lineWidth, d.strokeStyle = r.color;
                                    for (var g = t.getDistanceFromCenterForValue(i.reverse ? t.min : t.max), p = s(l.fontSize, a.defaultFontSize), m = s(l.fontStyle, a.defaultFontStyle), b = s(l.fontFamily, a.defaultFontFamily), v = e.fontString(p, m, b), x = t.getValueCount() - 1; x >= 0; x--) {
                                        if (r.display) {
                                            var y = t.getPointPosition(x, g);
                                            d.beginPath(), d.moveTo(t.xCenter, t.yCenter), d.lineTo(y.x, y.y), d.stroke(), d.closePath()
                                        }
                                        var k = t.getPointPosition(x, g + 5),
                                            S = s(l.fontColor, a.defaultFontColor);
                                        d.font = v, d.fillStyle = S;
                                        var w = t.pointLabels,
                                            C = this.getIndexAngle(x) + Math.PI / 2,
                                            M = 360 * C / (2 * Math.PI) % 360;
                                        0 === M || 180 === M ? d.textAlign = "center" : 180 > M ? d.textAlign = "left" : d.textAlign = "right", 90 === M || 270 === M ? d.textBaseline = "middle" : M > 270 || 90 > M ? d.textBaseline = "bottom" : d.textBaseline = "top", d.fillText(w[x] ? w[x] : "", k.x, k.y)
                                    }
                                }
                            }
                        }
                    });
                t.scaleService.registerScaleType("radialLinear", n, i)
            }
        }, {}],
        44: [function(t, e, a) {
            "use strict";
            var i = t(1);
            i = "function" == typeof i ? i : window.moment, e.exports = function(t) {
                var e = t.helpers,
                    a = {
                        units: [{
                            name: "millisecond",
                            steps: [1, 2, 5, 10, 20, 50, 100, 250, 500]
                        }, {
                            name: "second",
                            steps: [1, 2, 5, 10, 30]
                        }, {
                            name: "minute",
                            steps: [1, 2, 5, 10, 30]
                        }, {
                            name: "hour",
                            steps: [1, 2, 3, 6, 12]
                        }, {
                            name: "day",
                            steps: [1, 2, 5]
                        }, {
                            name: "week",
                            maxStep: 4
                        }, {
                            name: "month",
                            maxStep: 3
                        }, {
                            name: "quarter",
                            maxStep: 4
                        }, {
                            name: "year",
                            maxStep: !1
                        }]
                    },
                    n = {
                        position: "bottom",
                        time: {
                            parser: !1,
                            format: !1,
                            unit: !1,
                            round: !1,
                            displayFormat: !1,
                            isoWeekday: !1,
                            displayFormats: {
                                millisecond: "h:mm:ss.SSS a",
                                second: "h:mm:ss a",
                                minute: "h:mm:ss a",
                                hour: "MMM D, hA",
                                day: "ll",
                                week: "ll",
                                month: "MMM YYYY",
                                quarter: "[Q]Q - YYYY",
                                year: "YYYY"
                            }
                        },
                        ticks: {
                            autoSkip: !1
                        }
                    },
                    o = t.Scale.extend({
                        initialize: function() {
                            if (!i) throw new Error("Chart.js - Moment.js could not be found! You must include it before Chart.js to use the time scale. Download at https://momentjs.com");
                            t.Scale.prototype.initialize.call(this)
                        },
                        getLabelMoment: function(t, e) {
                            return "undefined" != typeof this.labelMoments[t] ? this.labelMoments[t][e] : null
                        },
                        getMomentStartOf: function(t) {
                            var e = this;
                            return "week" === e.options.time.unit && e.options.time.isoWeekday !== !1 ? t.clone().startOf("isoWeek").isoWeekday(e.options.time.isoWeekday) : t.clone().startOf(e.tickUnit)
                        },
                        determineDataLimits: function() {
                            var t = this;
                            t.labelMoments = [];
                            var a = [];
                            t.chart.data.labels && t.chart.data.labels.length > 0 ? (e.each(t.chart.data.labels, function(e) {
                                var i = t.parseTime(e);
                                i.isValid() && (t.options.time.round && i.startOf(t.options.time.round), a.push(i))
                            }, t), t.firstTick = i.min.call(t, a), t.lastTick = i.max.call(t, a)) : (t.firstTick = null, t.lastTick = null), e.each(t.chart.data.datasets, function(n, o) {
                                var r = [],
                                    l = t.chart.isDatasetVisible(o);
                                "object" == typeof n.data[0] && null !== n.data[0] ? e.each(n.data, function(e) {
                                    var a = t.parseTime(t.getRightValue(e));
                                    a.isValid() && (t.options.time.round && a.startOf(t.options.time.round), r.push(a), l && (t.firstTick = null !== t.firstTick ? i.min(t.firstTick, a) : a, t.lastTick = null !== t.lastTick ? i.max(t.lastTick, a) : a))
                                }, t) : r = a, t.labelMoments.push(r)
                            }, t), t.options.time.min && (t.firstTick = t.parseTime(t.options.time.min)), t.options.time.max && (t.lastTick = t.parseTime(t.options.time.max)), t.firstTick = (t.firstTick || i()).clone(), t.lastTick = (t.lastTick || i()).clone()
                        },
                        buildTicks: function() {
                            var i = this;
                            i.ctx.save();
                            var n = e.getValueOrDefault(i.options.ticks.fontSize, t.defaults.global.defaultFontSize),
                                o = e.getValueOrDefault(i.options.ticks.fontStyle, t.defaults.global.defaultFontStyle),
                                r = e.getValueOrDefault(i.options.ticks.fontFamily, t.defaults.global.defaultFontFamily),
                                l = e.fontString(n, o, r);
                            if (i.ctx.font = l, i.ticks = [], i.unitScale = 1, i.scaleSizeInUnits = 0, i.options.time.unit) i.tickUnit = i.options.time.unit || "day", i.displayFormat = i.options.time.displayFormats[i.tickUnit], i.scaleSizeInUnits = i.lastTick.diff(i.firstTick, i.tickUnit, !0), i.unitScale = e.getValueOrDefault(i.options.time.unitStepSize, 1);
                            else {
                                var s = i.isHorizontal() ? i.width - (i.paddingLeft + i.paddingRight) : i.height - (i.paddingTop + i.paddingBottom),
                                    d = i.tickFormatFunction(i.firstTick, 0, []),
                                    u = i.ctx.measureText(d).width,
                                    c = Math.cos(e.toRadians(i.options.ticks.maxRotation)),
                                    h = Math.sin(e.toRadians(i.options.ticks.maxRotation));
                                u = u * c + n * h;
                                var f = s / u;
                                i.tickUnit = "millisecond", i.scaleSizeInUnits = i.lastTick.diff(i.firstTick, i.tickUnit, !0), i.displayFormat = i.options.time.displayFormats[i.tickUnit];
                                for (var g = 0, p = a.units[g]; g < a.units.length;) {
                                    if (i.unitScale = 1, e.isArray(p.steps) && Math.ceil(i.scaleSizeInUnits / f) < e.max(p.steps)) {
                                        for (var m = 0; m < p.steps.length; ++m)
                                            if (p.steps[m] >= Math.ceil(i.scaleSizeInUnits / f)) {
                                                i.unitScale = e.getValueOrDefault(i.options.time.unitStepSize, p.steps[m]);
                                                break
                                            } break
                                    }
                                    if (p.maxStep === !1 || Math.ceil(i.scaleSizeInUnits / f) < p.maxStep) {
                                        i.unitScale = e.getValueOrDefault(i.options.time.unitStepSize, Math.ceil(i.scaleSizeInUnits / f));
                                        break
                                    }++g, p = a.units[g], i.tickUnit = p.name;
                                    var b = i.firstTick.diff(i.getMomentStartOf(i.firstTick), i.tickUnit, !0),
                                        v = i.getMomentStartOf(i.lastTick.clone().add(1, i.tickUnit)).diff(i.lastTick, i.tickUnit, !0);
                                    i.scaleSizeInUnits = i.lastTick.diff(i.firstTick, i.tickUnit, !0) + b + v, i.displayFormat = i.options.time.displayFormats[p.name]
                                }
                            }
                            var x;
                            if (i.options.time.min ? x = i.getMomentStartOf(i.firstTick) : (i.firstTick = i.getMomentStartOf(i.firstTick), x = i.firstTick), !i.options.time.max) {
                                var y = i.getMomentStartOf(i.lastTick),
                                    k = y.diff(i.lastTick, i.tickUnit, !0);
                                0 > k ? i.lastTick = i.getMomentStartOf(i.lastTick.add(1, i.tickUnit)) : k >= 0 && (i.lastTick = y), i.scaleSizeInUnits = i.lastTick.diff(i.firstTick, i.tickUnit, !0)
                            }
                            i.smallestLabelSeparation = i.width, e.each(i.chart.data.datasets, function(t, e) {
                                for (var a = 1; a < i.labelMoments[e].length; a++) i.smallestLabelSeparation = Math.min(i.smallestLabelSeparation, i.labelMoments[e][a].diff(i.labelMoments[e][a - 1], i.tickUnit, !0))
                            }, i), i.options.time.displayFormat && (i.displayFormat = i.options.time.displayFormat), i.ticks.push(i.firstTick.clone());
                            for (var S = 1; S <= i.scaleSizeInUnits; ++S) {
                                var w = x.clone().add(S, i.tickUnit);
                                if (i.options.time.max && w.diff(i.lastTick, i.tickUnit, !0) >= 0) break;
                                S % i.unitScale === 0 && i.ticks.push(w)
                            }
                            var C = i.ticks[i.ticks.length - 1].diff(i.lastTick, i.tickUnit);
                            (0 !== C || 0 === i.scaleSizeInUnits) && (i.options.time.max ? (i.ticks.push(i.lastTick.clone()), i.scaleSizeInUnits = i.lastTick.diff(i.ticks[0], i.tickUnit, !0)) : (i.ticks.push(i.lastTick.clone()), i.scaleSizeInUnits = i.lastTick.diff(i.firstTick, i.tickUnit, !0))), i.ctx.restore()
                        },
                        getLabelForIndex: function(t, e) {
                            var a = this,
                                i = a.chart.data.labels && t < a.chart.data.labels.length ? a.chart.data.labels[t] : "";
                            return "object" == typeof a.chart.data.datasets[e].data[0] && (i = a.getRightValue(a.chart.data.datasets[e].data[t])), a.options.time.tooltipFormat && (i = a.parseTime(i).format(a.options.time.tooltipFormat)), i
                        },
                        tickFormatFunction: function(t, a, i) {
                            var n = t.format(this.displayFormat),
                                o = this.options.ticks,
                                r = e.getValueOrDefault(o.callback, o.userCallback);
                            return r ? r(n, a, i) : n
                        },
                        convertTicksToLabels: function() {
                            var t = this;
                            t.tickMoments = t.ticks, t.ticks = t.ticks.map(t.tickFormatFunction, t)
                        },
                        getPixelForValue: function(t, e, a) {
                            var n = this;
                            t && t.isValid || (t = i(n.getRightValue(t)));
                            var o = t && t.isValid && t.isValid() ? t : n.getLabelMoment(a, e);
                            if (o) {
                                var r = o.diff(n.firstTick, n.tickUnit, !0),
                                    l = 0 !== r ? r / n.scaleSizeInUnits : r;
                                if (n.isHorizontal()) {
                                    var s = n.width - (n.paddingLeft + n.paddingRight),
                                        d = s * l + n.paddingLeft;
                                    return n.left + Math.round(d)
                                }
                                var u = n.height - (n.paddingTop + n.paddingBottom),
                                    c = u * l + n.paddingTop;
                                return n.top + Math.round(c)
                            }
                        },
                        getPixelForTick: function(t) {
                            return this.getPixelForValue(this.tickMoments[t], null, null)
                        },
                        getValueForPixel: function(t) {
                            var e = this,
                                a = e.isHorizontal() ? e.width - (e.paddingLeft + e.paddingRight) : e.height - (e.paddingTop + e.paddingBottom),
                                n = (t - (e.isHorizontal() ? e.left + e.paddingLeft : e.top + e.paddingTop)) / a;
                            return n *= e.scaleSizeInUnits, e.firstTick.clone().add(i.duration(n, e.tickUnit).asSeconds(), "seconds")
                        },
                        parseTime: function(t) {
                            var e = this;
                            return "string" == typeof e.options.time.parser ? i(t, e.options.time.parser) : "function" == typeof e.options.time.parser ? e.options.time.parser(t) : "function" == typeof t.getMonth || "number" == typeof t ? i(t) : t.isValid && t.isValid() ? t : "string" != typeof e.options.time.format && e.options.time.format.call ? (console.warn("options.time.format is deprecated and replaced by options.time.parser. See http://nnnick.github.io/Chart.js/docs-v2/#scales-time-scale"), e.options.time.format(t)) : i(t, e.options.time.format)
                        }
                    });
                t.scaleService.registerScaleType("time", o, n)
            }
        }, {
            1: 1
        }]
    }, {}, [7])(7)
});

ASM.storefinder = {
    storeData: "",
    storeId: "",
    originalStore: "",
    coords: {},
    storeSearchData: {},
    originAddress: "",
    autoLoad: function(data) {
        originAddress = data;
        ASM.storefinder.init();
        ASM.storefinder.bindStoreChange();
        ASM.storefinder.bindPagination();
    },
    createListItemHtml: function(data, id) {
        var item = "";
        item += '<li class="asm__list__entry">';
        item += '<input type="radio" name="storeNamePost" value="' + data.displayName + '" id="asm-store-filder-entry-' + id + '" class="js-asm-store-finder-input" data-id="' + id + '">';
        item += '<label for="asm-store-filder-entry-' + id + '" class="js-select-store-label">';
        item += '<span class="entry__info">';
        item += '<span class="entry__name">' + data.displayName + '</span>';
        item += '<span class="entry__address">' + data.line1 + ' ' + data.line2 + '</span>';
        item += '<span class="entry__city">' + data.town + '</span>';
        item += '</span>';
        item += '<span class="entry__distance">';
        item += '<span>' + data.formattedDistance + '</span>';
        item += '</span>';
        item += '</label>';
        item += '</li>';
        return item;
    },
    refreshNavigation: function() {
        var listitems = "";
        data = ASM.storefinder.storeData
        if (data) {
            for (i = 0; i < data["data"].length; i++) {
                listitems += ASM.storefinder.createListItemHtml(data["data"][i], i)
            }
            $(".js-asm-store-finder-navigation-list").html(listitems);
            var firstInput = $(".js-asm-store-finder-input")[0];
            $(firstInput).click();
        }
        var page = ASM.storefinder.storeSearchData.page;
        $(".js-asm-store-finder-pager-item-from").html(page * 10 + 1);
        var to = ((page * 10 + 10) > ASM.storefinder.storeData.total) ? ASM.storefinder.storeData.total : page * 10 + 10;
        $(".js-asm-store-finder-pager-item-to").html(to);
        $(".js-asm-store-finder-pager-item-all").html(ASM.storefinder.storeData.total);
        $(".js-asm-store-finder").removeClass("show-store");
    },
    bindPagination: function() {
        $(document).on("click", ".js-asm-store-finder-pager-prev", function(e) {
            e.preventDefault();
            var page = ASM.storefinder.storeSearchData.page;
            ASM.storefinder.getStoreData(page - 1)
            checkStatus(page - 1);
        })
        $(document).on("click", ".js-asm-store-finder-pager-next", function(e) {
            e.preventDefault();
            var page = ASM.storefinder.storeSearchData.page;
            ASM.storefinder.getStoreData(page + 1)
            checkStatus(page + 1);
        })

        function checkStatus(page) {
            if (page == 0) {
                $(".js-asm-store-finder-pager-prev").attr("disabled", "disabled")
            } else {
                $(".js-asm-store-finder-pager-prev").removeAttr("disabled")
            }
            if (page == Math.floor(ASM.storefinder.storeData.total / 10)) {
                $(".js-asm-store-finder-pager-next").attr("disabled", "disabled")
            } else {
                $(".js-asm-store-finder-pager-next").removeAttr("disabled")
            }
        }
    },
    bindStoreChange: function() {
        $(document).on("change", ".js-asm-store-finder-input", function(e) {
            e.preventDefault();
            storeData = ASM.storefinder.storeData["data"];
            var storeId = $(this).data("id");
            var $ele = $(".js-asm-store-finder-details");
            $.each(storeData[storeId], function(key, value) {
                if (key == "image") {
                    if (value != "") {
                        $ele.find(".js-asm-store-image").html('<img src="' + value + '" alt="" />');
                    } else {
                        $ele.find(".js-asm-store-image").html('');
                    }
                } else if (key == "productcode") {
                    $ele.find(".js-asm-store-productcode").val(value);
                } else if (key == "openings") {
                    if (value != "") {
                        var $oele = $ele.find(".js-asm-store-" + key);
                        var openings = "";
                        $.each(value, function(key2, value2) {
                            openings += "<dt>" + key2 + "</dt>";
                            openings += "<dd>" + value2 + "</dd>";
                        });
                        $oele.html(openings);
                    } else {
                        $ele.find(".js-asm-store-" + key).html('');
                    }
                } else if (key == "specialOpenings") {} else if (key == "features") {
                    var features = "";
                    $.each(value, function(key2, value2) {
                        features += "<li>" + value2 + "</li>";
                    });
                    $ele.find(".js-asm-store-" + key).html(features);
                } else {
                    if (value != "") {
                        $ele.find(".js-asm-store-" + key).html(value);
                    } else {
                        $ele.find(".js-asm-store-" + key).html('');
                    }
                }
            })
            ASM.storefinder.storeId = storeData[storeId];
            ASM.storefinder.initGoogleMap();
        })
    },
    initGoogleMap: function() {
        if ($(".js-asm-store-finder-map").length > 0) {
            ACC.global.addGoogleMapsApi("ASM.storefinder.loadGoogleMap");
        }
    },
    loadGoogleMap: function() {
        storeInformation = ASM.storefinder.storeId;
        if ($(".js-asm-store-finder-map").length > 0) {
            $(".js-asm-store-finder-map").attr("id", "asm-store-finder-map")
            var centerPoint = new google.maps.LatLng(storeInformation["latitude"], storeInformation["longitude"]);
            var mapOptions = {
                zoom: 16,
                zoomControl: true,
                panControl: true,
                streetViewControl: false,
                mapTypeId: google.maps.MapTypeId.ROADMAP,
                center: centerPoint
            }
            var map = new google.maps.Map(document.getElementById("asm-store-finder-map"), mapOptions);
            if (ASM.storefinder.originalStore == "") {
                ASM.storefinder.originalStore = data["data"][0];
            }
            if (ACC.config.googleApiKey != "" && ASM.storefinder.originalStore["latitude"] != storeInformation["latitude"]) {
                var directionsDisplay = new google.maps.DirectionsRenderer();
                var directionsService = new google.maps.DirectionsService();
                var originPoint = new google.maps.LatLng(ASM.storefinder.originalStore["latitude"], ASM.storefinder.originalStore["longitude"]);
                directionsDisplay.setMap(map);
                var request = {
                    origin: originPoint,
                    destination: centerPoint,
                    travelMode: 'DRIVING'
                };
                directionsService.route(request, function(response, status) {
                    if (status == 'OK') {
                        directionsDisplay.setDirections(response);
                    }
                });
            }
            var marker = new google.maps.Marker({
                position: new google.maps.LatLng(storeInformation["latitude"], storeInformation["longitude"]),
                map: map,
                title: storeInformation["name"],
                icon: "https://maps.google.com/mapfiles/marker" + 'A' + ".png"
            });
            var infowindow = new google.maps.InfoWindow({
                content: storeInformation["name"],
                disableAutoPan: true
            });
            google.maps.event.addListener(marker, 'click', function() {
                infowindow.open(map, marker);
            });
            var markerPosition = storeInformation["latitude"] + "," + storeInformation["longitude"];
            map.addListener('click', function(e) {
                if (ACC.config.googleApiKey != "") {
                    window.open("https://www.google.de/maps/dir/" + originAddress + "/" + markerPosition, '_blank');
                } else {
                    window.open("https://www.google.de/maps/dir/" + markerPosition, '_blank');
                }
            });
        }
    },
    bindSearch: function() {
        $(document).on("submit", '#storeFinderForm', function(e) {
            e.preventDefault()
            var q = $(".js-asm-store-finder-search-input").val();
            if (q.length > 0) {
                ASM.storefinder.getInitStoreData(q);
            } else {
                if ($(".js-asm-storefinder-alert").length < 1) {
                    var emptySearchMessage = $(".btn-primary").data("searchEmpty")
                    $(".js-asm-store-finder").hide();
                    $("#storeFinder").before('<div class="js-asm-storefinder-alert alert alert-danger alert-dismissable" ><button class="close" type="button" data-dismiss="alert" aria-hidden="true">×</button>' + emptySearchMessage + '</div>');
                }
            }
        })
        $(".js-asm-store-finder").hide();
        $(document).on("click", '#findStoresNearMe', function(e) {
            e.preventDefault()
            ASM.storefinder.getInitStoreData(null, ASM.storefinder.coords.latitude, ASM.storefinder.coords.longitude);
        })
    },
    getStoreData: function(page) {
        ASM.storefinder.storeSearchData.page = page;
        url = $(".js-asm-store-finder").data("url");
        $.ajax({
            url: url,
            data: ASM.storefinder.storeSearchData,
            type: "get",
            success: function(response) {
                ASM.storefinder.storeData = $.parseJSON(response);
                ASM.storefinder.refreshNavigation();
                if (ASM.storefinder.storeData.total < 10) {
                    $(".js-asm-store-finder-pager-next").attr("disabled", "disabled");
                }
            }
        });
    },
    getInitStoreData: function(q, latitude, longitude) {
        $(".alert").remove();
        data = {
            "q": "",
            "page": 0
        }
        if (q != null) {
            data.q = q;
        }
        if (latitude != null) {
            data.latitude = latitude;
        }
        if (longitude != null) {
            data.longitude = longitude;
        }
        ASM.storefinder.storeSearchData = data;
        ASM.storefinder.getStoreData(data.page);
        $(".js-asm-store-finder").show();
        $(".js-asm-store-finder-pager-prev").attr("disabled", "disabled")
        $(".js-asm-store-finder-pager-next").removeAttr("disabled")
    },
    init: function() {
        $("#findStoresNearMe").attr("disabled", "disabled");
        if (navigator.geolocation) {
            navigator.geolocation.getCurrentPosition(function(position) {
                ASM.storefinder.coords = position.coords;
                $('#findStoresNearMe').removeAttr("disabled");
            }, function(error) {
                console.log("An error occurred... The error code and message are: " + error.code + "/" + error.message);
            });
        }
    }
};

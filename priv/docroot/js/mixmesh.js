var Mixmesh = (function() {
    return {
        setHeight: function(targetId, siblingIds) {
            var targetHeight = window.innerHeight;
            for (i = 0; i < siblingIds.length; i++) {
                targetHeight -= $(siblingIds[i]).outerHeight(true);
            }
            $(targetId).outerHeight(targetHeight.toString() + "px", true);
        },
        get: function(url, success) {
            $.get(url, null, success);
        },
        post: function(url, data, success, error) {
            $.ajax(url, {
                data: JSON.stringify(data),
                contentType: "application/json",
                type: "POST",
                success: success,
                error: error
            });
        },
        put: function(url, data, success, error) {
            $.ajax(url, {
                data: JSON.stringify(data),
                contentType: "application/json",
                type: "PUT",
                success: success,
                error: error
            });
        },
        setClass: function(id, newClass, oldClass) {
            if (!$(id).hasClass(newClass)) {
                $(id).removeClass(oldClass);
                $(id).addClass(newClass);
            }
        },
        formatAmount: function(n, what) {
            if (typeof n == "string") {
                n = parseInt(n);
            }
            if (n == 0) {
                return "no " + what + "s";
            } else if (n == 1) {
                return "one " + what;
            } else {
                return n.toString() + " " + what + "s";
            }
        },
        validPassword: function(id) {
            Mixmesh.setClass(id, "uk-form-success", "uk-form-danger");
        },
        invalidPassword: function(id) {
            Mixmesh.setClass(id, "uk-form-danger", "uk-form-success");
        },
        passwordKeyupHandler: function(id, callback) {
            var idAgain = id + "-again";
            var handler = function() {
                if ($(id).val().length < 6) {
                    Mixmesh.invalidPassword(id);
                    Mixmesh.invalidPassword(idAgain);
                } else {
                    Mixmesh.validPassword(id);
                    if ($(id).val() == $(idAgain).val()) {
                        Mixmesh.validPassword(idAgain);
                    } else {
                        Mixmesh.invalidPassword(idAgain);
                    }
                }
                callback();
            }
            return handler;
        },
        passwordLockHandler: function(id) {
            var handler = function() {
                if ($(id).attr("type") == "password") {
                    $(id).attr("type", "text");
                    $(id + "-again").attr("type", "text");
                    $(this).attr("uk-icon", "icon: unlock");
                } else {
                    $(id).attr("type", "password");
                    $(id + "-again").attr("type", "password");
                    $(this).attr("uk-icon", "icon: lock");
                }
            }
            return handler;
        },
        showGenericDialog: function(params) {
            if (params.title) {
                $("#generic-dialog-title").text(params.title);
                $("#generic-dialog-title").show();
            } else {
                $("#generic-dialog-title").hide();
            }

            if (params.oncancel) {
                $("#generic-dialog-cancel").click(params.oncancel);
                $("#generic-dialog-cancel").show();
            } else {
                $("#generic-dialog-cancel").hide();
            }

            if (params.onok) {
                $("#generic-dialog-ok").click(params.onok);
                $("#generic-dialog-ok").show();
            } else {
                $("#generic-dialog-ok").hide();
            }

            if (params.oncancel && params.onok) {
                Mixmesh.setClass("#generic-dialog-ok", "uk-button-primary",
                                 "uk-button-default");
            } else {
                Mixmesh.setClass("#generic-dialog-ok", "uk-button-default",
                                 "uk-button-primary");
            }

            if (params.content) {
                $("#generic-dialog-content").html(params.content);
                $("#generic-dialog-content").show();
            } else {
                $("#generic-dialog-content").hide();
            }

            UIkit.modal("#generic-dialog").show();
        },
        hideGenericDialog: function(params) {
            UIkit.modal("#generic-dialog").hide();
        },
        formatError: function(jqXHR, _textStatus, _errorThrown) {
            if (jqXHR.response && jqXHR.response.length > 0) {
                return jqXHR.response;
            } else {
                if (jqXHR.readyState == 4 && jqXHR.status == 0) {
                    return "Network error";
                } else {
                    return "Internal error";
                }
            }
        }
    };
})();

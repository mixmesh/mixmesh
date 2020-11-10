var Mixmesh = (function() {
    var validPassword = function(id) {
        Mixmesh.setClass(id, "uk-form-success", "uk-form-danger");
    };
    
    var invalidPassword = function(id) {
        Mixmesh.setClass(id, "uk-form-danger", "uk-form-success");
    };
    
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
        setClass: function(id, newClass, oldClass) {
            if (!$(id).hasClass(newClass)) {
                $(id).removeClass(oldClass);
                $(id).addClass(newClass);
            }
        },
        passwordKeyupHandler: function(id, callback) {
            var handler = function() {
                if ($(id).val().length >= 6) {
                    validPassword(id);
                } else {
                    invalidPassword(id);
                }
                callback();
            };
            return handler;
        },
        passwordAgainKeyupHandler: function(id, callback) {
            var handler = function() {
                if ($(id).val() == $(id + "-again").val() &&
                    $(id).val().length >= 6) {
                    validPassword(id + "-again");
                } else {
                    invalidPassword(id + "-again");
                }
                callback();
            };
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
        formatXHRError: function(xhr) {
            if (xhr.response && xhr.response.length > 0) {
                return xhr.response;
            } else {
                if (xhr.readyState == 4 && xhr.status == 0) {
                    return "Network error";
                } else {
                    return "Internal error";
                }
            }
        }
    };
})();

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
        setClass: function(id, newClass, oldClass) {
            if (!$(id).hasClass(newClass)) {
                $(id).removeClass(oldClass);
                $(id).addClass(newClass);
            }
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
        }
    };
})();

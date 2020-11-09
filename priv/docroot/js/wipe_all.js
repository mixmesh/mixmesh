var WipeAll = (function() {
    var validPassword =
        function(id) {
            WipeAll.setClass(id, "uk-form-success", "uk-form-danger");
            WipeAll.setClass(id + "-again", "uk-form-success", "uk-form-danger");
        };
    
    var invalidPassword =
        function(id) {
            WipeAll.setClass(id, "uk-form-danger", "uk-form-success");
            WipeAll.setClass(id + "-again",  "uk-form-danger", "uk-form-success");
        };
    
    return {
        setClass: function(id, newClass, oldClass) {
            if (!$(id).hasClass(newClass)) {
                $(id).removeClass(oldClass);
                $(id).addClass(newClass);
            }
        },
        toggleWipeButton: function() {
            if ($("#pseudonym").hasClass("uk-form-success") &&
                $("#mail-password").hasClass("uk-form-success") &&
                $("#http-password").hasClass("uk-form-success")) {
                if ($("#wipe-button").prop('disabled')) {
                    $("#wipe-button").prop('disabled', false);
                }
            } else {
                if (!$("#wipe-button").prop('disabled')) {
                    $("#wipe-button").prop('disabled', true);
                }
            }
        },
        passwordKeyupHandler: function(id) {
            var handler =
                function() {
                    if ($(id).val().length < 6) {
                        invalidPassword(id);
                    } else {
                        if ($(id).val() == $(id + "-again").val()) {
                            validPassword(id);
                        } else {
                            invalidPassword(id);
                        }
                    }
                    WipeAll.toggleWipeButton();
                };
            return handler;
        },
        passwordLockHandler: function(id) {
            var handler =
                function() {
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
        }
    };
})();

$(document).ready(function() {
    Mixmesh.setHeight("#content", ["#navigation"]);
    
    $("#pseudonym").keyup(
        function() {
            if ($(this).val().length < 6) {
                WipeAll.setClass(this, "uk-form-danger", "uk-form-success");
            } else {
                WipeAll.setClass(this, "uk-form-success", "uk-form-danger");
            }
            WipeAll.toggleWipeButton();
        });    
    $("#mail-password").keyup(WipeAll.passwordKeyupHandler("#mail-password"));
    $("#mail-password-again").keyup(WipeAll.passwordKeyupHandler("#mail-password"));
    $("#http-password").keyup(WipeAll.passwordKeyupHandler("#http-password"));
    $("#http-password-again").keyup(WipeAll.passwordKeyupHandler("#http-password"));    
    $("#mail-password-lock").click(WipeAll.passwordLockHandler("#mail-password"));
    $("#http-password-lock").click(WipeAll.passwordLockHandler("#http-password"));
    $("#wipe-button").click(
        function() {
            $("#wipe-button").prop('disabled', true);            
            Mixmesh.post(
                "/dj/system/wipe",
                {
                    nym: $("#pseudonym").val(),
                    "smtp-password": $("#mail-password").val(),
                    "pop3-password": $("#mail-password").val(),
                    "http-password": $("#http-password").val()
                },
                function(data, textStatus, _jqXHR) {
                    console.log("/dj/wipe (POST) succeeded");
                    console.log(data);
                    $("#meta-content").load(
                        "/wipe-2.html #content",
                        function() {
                            new QRCode($("#qrcode").get(0), {
	                        text: data["public-key"] + data["secret-key"],
	                        width: 800,
	                        height: 800,
	                        colorDark : "#000000",
	                        colorLight : "#ffffff",
	                        correctLevel : QRCode.CorrectLevel.H
                            });
                            $("#next-button").click(function() {
                                alert(3);
                            });
                            Mixmesh.setHeight("#meta-content", ["#navigation"]);
                        });
                },
                function(_jqXHR, textStatus, errorThrown) {
                    console.log("/dj/wipe (POST) failed");
                    console.log("textStatus: " + textStatus);
                    console.log("errorThrown: " + errorThrown);
                });
        });
});

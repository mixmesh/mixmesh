var Wipe = (function() {
    var validPassword =
        function(id) {
            Wipe.setClass(id, "uk-form-success", "uk-form-danger");
            Wipe.setClass(id + "-again", "uk-form-success", "uk-form-danger");
        };
    
    var invalidPassword =
        function(id) {
            Wipe.setClass(id, "uk-form-danger", "uk-form-success");
            Wipe.setClass(id + "-again",  "uk-form-danger", "uk-form-success");
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
                    Wipe.toggleWipeButton();
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
    $("#pseudonym").keyup(
        function() {
            if ($(this).val().length < 6) {
                Wipe.setClass(this, "uk-form-danger", "uk-form-success");
            } else {
                Wipe.setClass(this, "uk-form-success", "uk-form-danger");
            }
            Wipe.toggleWipeButton();
        });    
    $("#mail-password").keyup(Wipe.passwordKeyupHandler("#mail-password"));
    $("#mail-password-again")
        .keyup(Wipe.passwordKeyupHandler("#mail-password"));
    $("#http-password").keyup(Wipe.passwordKeyupHandler("#http-password"));
    $("#http-password-again")
        .keyup(Wipe.passwordKeyupHandler("#http-password"));    
    $("#mail-password-lock").click(Wipe.passwordLockHandler("#mail-password"));
    $("#http-password-lock").click(Wipe.passwordLockHandler("#http-password"));
    $("#wipe-button").click(
        function() {
            $("#wipe-button").prop('disabled', true);

            Wipe.pseudonym = $("#pseudonym").val();
            Wipe.mailPassword = $("#mail-password").val();

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

                    // Disable top-level navigation bar
                    $("#navbar-wipe a").removeAttr("href");    
                    $("#navbar-wipe").removeClass("uk-active");
                    $("#navbar-reinstall a").removeAttr("href");    
                    
                    // Load step 2
                    $("#meta-content").load(
                        "/wipe-2.html #content",
                        function() {
                            $("#meta-content").hide(); // To avoid flicker (see below)
                            new QRCode($("#qrcode").get(0), {
	                        text: data["public-key"] + data["secret-key"],
	                        width: 800,
	                        height: 800,
	                        colorDark : "#000000",
	                        colorLight : "#ffffff",
	                        correctLevel : QRCode.CorrectLevel.H
                            });
                            // To avoid flicker (see above)
                            setTimeout(function() {
                                $("#meta-content").show();
                            }, 10);
                            $("#next-button").click(function() {
                                // Load step 3
                                $("#meta-content").load(
                                    "/wipe-3.html #content",
                                    function() {
                                        $("#email-address")
                                            .val(Wipe.pseudonym +
                                                 "@mixmesh.net");
                                        var ip_port = data["smtp-address"]
                                            .split(":");
                                        $("#smtp-ip-address")
                                            .val(ip_port[0]);
                                        $("#smtp-port").val(ip_port[1]);
                                        ip_port = data["pop3-address"]
                                            .split(":");
                                        $("#pop3-ip-address").val(ip_port[0]);
                                        $("#pop3-port").val(ip_port[1]);
                                        $("#mail-password")
                                            .val(Wipe.mailPassword);
                                    });
                            });
                        });
                },
                function(_jqXHR, textStatus, errorThrown) {
                    console.log("/dj/wipe (POST) failed");
                    console.log("textStatus: " + textStatus);
                    console.log("errorThrown: " + errorThrown);
                });
        });
});

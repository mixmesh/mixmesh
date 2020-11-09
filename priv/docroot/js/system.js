var System = (function() {
    var validPassword =
        function(id) {
            $(id).removeClass("uk-form-danger")
            $(id + "-again").removeClass("uk-form-danger")
        };
    
    var invalidPassword =
        function(id) {
            if (!$(id).hasClass("uk-form-danger")) {
                $(id).addClass("uk-form-danger");
                $(id + "-again").addClass("uk-form-danger");
            }
        };

    return {
        passwordKeyupHandler: function(id, passwordChanged) {
            var handler =
                function() {
                    if ($(id).val().length < 6) {
                        invalidPassword(id);
                    } else {
                        if ($(id).val() == $(id + "-again").val()) {
                            passwordChanged($(id).val());
                        } else {
                            invalidPassword(id);
                        }
                    }
                };
            return handler;
        },
        mailPasswordChanged: function(password) {
            Mixmesh.post(
                "/dj/edit-config",
                {
                    "player": {
                        "smtp-server": {
                            "password-digest": btoa(password)
                        },
                        "pop3-server": {
                            "password-digest": btoa(password)
                        }
                    }
                },
                function(data, textStatus, _jqXHR) {
                    console.log("/dj/edit-config (POST) succeeded");
                    console.log(data);
                    validPassword("#mail-password");
                },
                function(_jqXHR, textStatus, errorThrown) {
                    console.log("/dj/edit-config (POST) failed");
                    console.log("textStatus: " + textStatus);
                    console.log("errorThrown: " + errorThrown);
                });
        },
        httpPasswordChanged: function(password) {
            Mixmesh.post(
                "/dj/edit-config",
                {
                    "player": {
                        "http-server": {
                            "password": password
                        }
                    }
                },
                function(data, textStatus, _jqXHR) {
                    console.log("/dj/edit-config (POST) succeeded");
                    console.log(data);
                    validPassword("#http-password");
                    validPassword("#http-password");
                },
                function(_jqXHR, textStatus, errorThrown) {
                    console.log("/dj/edit-config (POST) failed");
                    console.log("textStatus: " + textStatus);
                    console.log("errorThrown: " + errorThrown);
                });
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
    Mixmesh.setHeight("#meta-content", ["#navigation"]);
    
    $("#mail-password").keyup(
        System.passwordKeyupHandler(
            "#mail-password", System.mailPasswordChanged));
    $("#mail-password-again").keyup(
        System.passwordKeyupHandler(
            "#mail-password", System.mailPasswordChanged));
    $("#http-password").keyup(
        System.passwordKeyupHandler(
            "#http-password", System.httpPasswordChanged));
    $("#http-password-again").keyup(
        System.passwordKeyupHandler(
            "#http-password", System.httpPasswordChanged));
    $("#mail-password-lock").click(
        System.passwordLockHandler("#mail-password"));
    $("#http-password-lock").click(
        System.passwordLockHandler("#http-password"));
    
    Mixmesh.post(
        "/dj/get-config",
        {
            "player": {
                "smtp-server": {
                    address: true
                },
                "pop3-server": {
                    address: true
                }
            }
        },
        function(data, textStatus, _jqXHR) {
            console.log("/dj/get-config (POST) succeeded");
            console.log(data);
            
            // SMTP server
            var ip_port = data.player["smtp-server"].address.split(":");
            $("#smtp-ip-address").val(ip_port[0]);
            $("#smtp-port").val(ip_port[1]);

            // POP3 server
            ip_port = data.player["pop3-server"].address.split(":");
            $("#pop3-ip-address").val(ip_port[0]);
            $("#pop3-port").val(ip_port[1]);
        },
        function(_jqXHR, textStatus, errorThrown) {
            console.log("/dj/get-config (POST) failed");
            console.log("textStatus: " + textStatus);
            console.log("errorThrown: " + errorThrown);
        });
});

var System = (function() {
    var notChangedPassword = function(id) {
        $(id).removeClass("uk-form-danger").removeClass("uk-form-success")
    };
    
    return {
        passwordKeyupHandler: function(id, updatePassword) {
            var idAgain = id + "-again";
            var handler = function() {
                if ($(id).val().length == 0 && $(idAgain).val().length == 0) {
                    notChangedPassword(id);
                    notChangedPassword(idAgain);
                } else {
                    if ($(id).val().length < 6) {
                        Mixmesh.invalidPassword(id);
                        Mixmesh.invalidPassword(idAgain);
                    } else {
                        if ($(id).val() == $(idAgain).val()) {
                            $(id).prop("disabled", true);
                            $(idAgain).prop("disabled", true);
                            updatePassword($(id).val());
                        } else {
                            Mixmesh.invalidPassword(id);
                            Mixmesh.invalidPassword(idAgain);
                        }
                    }
                }
            }
            return handler;
        },
        updateMailPassword: function(password) {
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
                    notChangedPassword("#mail-password");
                    notChangedPassword("#mail-password-again");
                    Mixmesh.showGenericDialog({
                        title: "Success",
                        content: "<p>Mail password has been updated</p>",
                        onok: function() {
                            Mixmesh.hideGenericDialog();
                        }
                    });
                    setTimeout(function() {
                        $("#mail-password").prop("disabled", false);
                        $("#mail-password-again").prop("disabled", false);
                    }, 2000);
                },
                function(jqXHR, textStatus, errorThrown) {
                    console.log("/dj/edit-config (POST) failed");
                    console.log("textStatus: " + textStatus);
                    console.log("errorThrown: " + errorThrown);
                    Mixmesh.showGenericDialog({
                        title: "System not available",
                        content: "<p>" + Mixmesh.formatXHRError(jqXHR) + "</p>",
                        onok: function() {
                            Mixmesh.hideGenericDialog();
                        }
                    });
                    setTimeout(function() {
                        $("#mail-password").prop("disabled", false);
                        $("#mail-password-again").prop("disabled", false);
                    }, 2000);
                });
        },
        updateHTTPPassword: function(password) {
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
                    notChangedPassword("#http-password");
                    notChangedPassword("#http-password-again");
                    Mixmesh.showGenericDialog({
                        title: "Success",
                        content: "<p>HTTP password has been updated</p>",
                        onok: function() {
                            Mixmesh.hideGenericDialog();
                        }
                    });
                    setTimeout(function() {
                        $("#http-password").prop("disabled", false);
                        $("#http-password-again").prop("disabled", false);
                    }, 2000);
                },
                function(jqXHR, textStatus, errorThrown) {
                    console.log("/dj/edit-config (POST) failed");
                    console.log("textStatus: " + textStatus);
                    console.log("errorThrown: " + errorThrown);
                    Mixmesh.showGenericDialog({
                        "title": "System not available",
                        content: "<p>" + Mixmesh.formatXHRError(jqXHR) + "</p>",
                        onok: function() {
                            Mixmesh.hideGenericDialog();
                        }
                    });
                    setTimeout(function() {
                        $("#http-password").prop("disabled", false);
                        $("#http-password-again").prop("disabled", false);
                    }, 2000);
                });
        }
    };
})();

$(document).ready(function() {
    $("#mail-password").keyup(
        System.passwordKeyupHandler(
            "#mail-password", System.updateMailPassword));
    $("#mail-password-again").keyup(
        System.passwordKeyupHandler(
            "#mail-password", System.updateMailPassword));
    $("#http-password").keyup(
        System.passwordKeyupHandler(
            "#http-password", System.updateHTTPPassword));
    $("#http-password-again").keyup(
        System.passwordKeyupHandler(
            "#http-password", System.updateHTTPPassword));
    $("#mail-password-lock").click(
        Mixmesh.passwordLockHandler("#mail-password"));
    $("#http-password-lock").click(
        Mixmesh.passwordLockHandler("#http-password"));

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
        function(jqXHR, textStatus, errorThrown) {
            console.log("/dj/get-config (POST) failed");
            console.log("textStatus: " + textStatus);
            console.log("errorThrown: " + errorThrown);

            Mixmesh.showGenericDialog({
                "title": "System not available",
                content: "<p>" + Mixmesh.formatXHRError(jqXHR) + "</p>",
                onok: function() {
                    Mixmesh.hideGenericDialog();
                }
            });
        });
});

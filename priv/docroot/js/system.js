var System = (function() {
    return {
        lockOwner: null,
        passwordKeyupHandler: function(id, updatePassword) {
            var idAgain = id + "-again";
            var handler = function() {
                if ($(id).val().length == 0 && $(idAgain).val().length == 0) {
                    Mixmesh.clearInput(id);
                    Mixmesh.clearInput(idAgain);
                } else {
                    if ($(id).val().length < 6) {
                        Mixmesh.invalidInput(id);
                        Mixmesh.invalidInput(idAgain);
                    } else {
                        if ($(id).val() == $(idAgain).val()) {
                            $(id).prop("disabled", true);
                            $(idAgain).prop("disabled", true);
                            updatePassword($(id).val());
                        } else {
                            Mixmesh.validInput(id);
                        }
                    }
                }
            }
            return handler;
        },
        locationKeyupHandler: function(id) {
            var handler = function() {
                if (isNaN($(id).val())) {
                    Mixmesh.invalidInput(id);
                } else if ($(id).val() == $(id).data("current-value")) {
                    Mixmesh.clearInput(id);
                } else {
                    Mixmesh.validInput(id);
                }
                if ($("#longitude").hasClass("uk-form-success") ||
                    $("#latitude").hasClass("uk-form-success")) {
                    $("#apply-location-button").prop("disabled", false);
                } else {
                    $("#apply-location-button").prop("disabled", true);
                }
            };
            return handler;
        },
        updateMailPassword: function(password) {
            Mixmesh.post(
                "/edit-config",
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
                    console.log("/edit-config (POST) succeeded");
                    console.log(data);
                    Mixmesh.clearInput("#mail-password");
                    Mixmesh.clearInput("#mail-password-again");
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
                    console.log("/edit-config (POST) failed");
                    console.log("textStatus: " + textStatus);
                    console.log("errorThrown: " + errorThrown);
                    Mixmesh.showGenericDialog({
                        title: "System not available",
                        content: "<p>" + Mixmesh.formatError(
                            jqXHR, textStatus, errorThrown) + "</p>",
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
                "/edit-config",
                {
                    "player": {
                        "http-server": {
                            "password": password
                        }
                    }
                },
                function(data, textStatus, _jqXHR) {
                    console.log("/edit-config (POST) succeeded");
                    console.log(data);
                    Mixmesh.clearInput("#http-password");
                    Mixmesh.clearInput("#http-password-again");
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
                    console.log("/edit-config (POST) failed");
                    console.log("textStatus: " + textStatus);
                    console.log("errorThrown: " + errorThrown);
                    Mixmesh.showGenericDialog({
                        "title": "System not available",
                        content: "<p>" + Mixmesh.formatError(
                            jqXHR, textStatus, errorThrown) + "</p>",
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
    $("#mail-password")
        .keyup(System.passwordKeyupHandler(
            "#mail-password", System.updateMailPassword));
    $("#mail-password-again")
        .keyup(System.passwordKeyupHandler(
            "#mail-password", System.updateMailPassword));
    $("#http-password")
        .keyup(System.passwordKeyupHandler(
            "#http-password", System.updateHTTPPassword));
    $("#http-password-again")
        .keyup(System.passwordKeyupHandler(
            "#http-password", System.updateHTTPPassword));
    $("#mail-password-lock")
        .click(Mixmesh.passwordLockHandler("#mail-password"));
    $("#http-password-lock")
        .click(Mixmesh.passwordLockHandler("#http-password"));
    $("#longitude").keyup(System.locationKeyupHandler("#longitude"));
    $("#latitude").keyup(System.locationKeyupHandler("#latitude"));
    $("#use-gps")
        .click(function() {
            $("#static-location").hide();
            Mixmesh.post(
                "/edit-config",
                {
                    "player": {
                        "routing": {
                            "type": "location",
                            "use-gps": true
                        }
                    }
                },
                function(data, textStatus, _jqXHR) {
                    console.log("/edit-config (POST) succeeded");
                    console.log(data);                    
                },
                function(jqXHR, textStatus, errorThrown) {
                    console.log("/edit-config (POST) failed");
                    console.log("textStatus: " + textStatus);
                    console.log("errorThrown: " + errorThrown);
                    Mixmesh.showGenericDialog({
                        title: "System not available",
                        content: "<p>" + Mixmesh.formatError(
                            jqXHR, textStatus, errorThrown) + "</p>",
                        onok: function() {
                            Mixmesh.hideGenericDialog();
                        }
                    });
                });
        });
    $("#use-static-location")
        .click(function() {
            $("#static-location").show();
            var longitude = Number($("#longitude").val());
            var latitude = Number($("#latitude").val());
            Mixmesh.post(
                "/edit-config",
                {
                    "player": {
                        "routing": {
                            "type": "location",
                            "use-gps": false,
                            "longitude": longitude,
                            "latitude": latitude
                        }
                    }
                },
                function(data, textStatus, _jqXHR) {
                    console.log("/edit-config (POST) succeeded");
                    console.log(data);
                    $("#apply-location-button").prop("disabled", true);
                    Mixmesh.clearInput("#longitude");
                    $("#longitude").data("current-value", longitude);
                    Mixmesh.clearInput("#latitude");
                    $("#latitude").data("current-value", latitude);
                },
                function(jqXHR, textStatus, errorThrown) {
                    console.log("/edit-config (POST) failed");
                    console.log("textStatus: " + textStatus);
                    console.log("errorThrown: " + errorThrown);
                    Mixmesh.showGenericDialog({
                        title: "System not available",
                        content: "<p>" + Mixmesh.formatError(
                            jqXHR, textStatus, errorThrown) + "</p>",
                        onok: function() {
                            Mixmesh.hideGenericDialog();
                        }
                    });
                });
        });
    $("#blind-routing")
        .click(function() {
            $("#static-location").hide();
            Mixmesh.post(
                "/edit-config",
                {
                    "player": {
                        "routing": {
                            "type": "blind",
                            "use-gps": false
                        }
                    }
                },
                function(data, textStatus, _jqXHR) {
                    console.log("/edit-config (POST) succeeded");
                    console.log(data);                    
                },
                function(jqXHR, textStatus, errorThrown) {
                    console.log("/edit-config (POST) failed");
                    console.log("textStatus: " + textStatus);
                    console.log("errorThrown: " + errorThrown);
                    Mixmesh.showGenericDialog({
                        title: "System not available",
                        content: "<p>" + Mixmesh.formatError(
                            jqXHR, textStatus, errorThrown) + "</p>",
                        onok: function() {
                            Mixmesh.hideGenericDialog();
                        }
                    });
                });
        });        
    if (navigator.geolocation) {
        $("#get-location-button").prop("disabled", false);
        $("#get-location-button")
            .click(function() {
                var onSuccess = function(position) {
                    $("#longitude").val(position.coords.longitude);
                    (System.locationKeyupHandler("#longitude"))();
                    $("#latitude").val(position.coords.latitude);
                    (System.locationKeyupHandler("#latitude"))();
                    Mixmesh.unlockScreen(System.lockOwner);
                };
                var onError = function(error) {
                    var reason;
                    switch (error.code) {
                    case 0:
                        reason = "Reason: Unknown error";
                        break;
                    case 1:
                        reason = "Reason: Permission denied";
                        break;
                    case 2:
                        reason = "Reason: Position unavailable";
                        break;
                    case 3:
                        reason = "Reason: Timeout";
                        break;
                    default:
                        reason = "Reason: Internal error";
                    }
                    Mixmesh.showGenericDialog({
                        title: "Static location not available",
                        content: "<p>" + reason + "</p>",
                        onok: function() {
                            Mixmesh.unlockScreen(System.lockOwner);
                            Mixmesh.hideGenericDialog();
                        }
                    });
                };
                System.lockOwner = Mixmesh.lockScreen();
                navigator.geolocation.getCurrentPosition(onSuccess, onError);
                return false;
            });
    }
    $("#apply-location-button")
        .click(function() {
            var longitude = Number($("#longitude").val());
            var latitude = Number($("#latitude").val());
            Mixmesh.post(
                "/edit-config",
                {
                    "player": {
                        "routing": {
                            "longitude": longitude,
                            "latitude": latitude
                        }
                    }
                },
                function(data, textStatus, _jqXHR) {
                    console.log("/edit-config (POST) succeeded");
                    console.log(data);
                    $("#apply-location-button").prop("disabled", true);
                    Mixmesh.clearInput("#longitude");
                    $("#longitude").data("current-value", longitude);
                    Mixmesh.clearInput("#latitude");
                    $("#latitude").data("current-value", latitude);
                },
                function(jqXHR, textStatus, errorThrown) {
                    console.log("/edit-config (POST) failed");
                    console.log("textStatus: " + textStatus);
                    console.log("errorThrown: " + errorThrown);
                    Mixmesh.showGenericDialog({
                        title: "System not available",
                        content: "<p>" + Mixmesh.formatError(
                            jqXHR, textStatus, errorThrown) + "</p>",
                        onok: function() {
                            Mixmesh.hideGenericDialog();
                        }
                    });
                });
        });
    
    Mixmesh.post(
        "/get-config",
        {
            "player": {
                "routing": {
                    "type": true,
                    "use-gps": true,
                    longitude: true,
                    latitude: true
                },
                "smtp-server": {
                    address: true
                },
                "pop3-server": {
                    address: true
                },
                "http-server": {
                    address: true
                }
            }
        },
        function(data, textStatus, _jqXHR) {
            console.log("/get-config (POST) succeeded");
            console.log(data);

            // Routing
            var routing = data.player.routing;
            $("#longitude").val(routing.longitude);
            $("#longitude").data("current-value", routing.longitude);
            $("#latitude").val(routing.latitude);
            $("#latitude").data("current-value", routing.latitude);
            if (routing.type == "blind") {
                $("#blind-routing").prop("checked", true);
            } else if (routing["use-gps"]) {
                $("#use-gps").prop("checked", true);
            } else {
                $("#use-static-location").prop("checked", true);
                $("#static-location").show();
            }
            
            // SMTP server
            var ip_port = data.player["smtp-server"].address.split(":");
            $("#smtp-ip-address").val(ip_port[0]);
            $("#smtp-port").val(ip_port[1]);

            // POP3 server
            ip_port = data.player["pop3-server"].address.split(":");
            $("#pop3-ip-address").val(ip_port[0]);
            $("#pop3-port").val(ip_port[1]);

            // HTTP server (USB)
            ip_port = data.player["http-server"].address[0].split(":");
            $("#http-ip-address-usb").val(ip_port[0]);
            $("#http-port-usb").val(ip_port[1]);

            // HTTP server (Bluetooth)
            ip_port = data.player["http-server"].address[1].split(":");
            $("#http-ip-address-bluetooth").val(ip_port[0]);
            $("#http-port-bluetooth").val(ip_port[1]);
        },
        function(jqXHR, textStatus, errorThrown) {
            console.log("/get-config (POST) failed");
            console.log("textStatus: " + textStatus);
            console.log("errorThrown: " + errorThrown);

            Mixmesh.showGenericDialog({
                title: "System not available",
                content: "<p>" + Mixmesh.formatError(
                    jqXHR, textStatus, errorThrown) + "</p>",
                onok: function() {
                    Mixmesh.hideGenericDialog();
                }
            });
        });
});

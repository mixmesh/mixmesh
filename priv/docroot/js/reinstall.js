var Reinstall = (function() {
    var routingType = "location";
    var useGPS = true;
    var longitude = 0;
    var latitude = 0;
    var lockOwner;

    var toggleReinstallButton = function() {
        if ($("#pin").hasClass("uk-form-success") &&
            $("#mail-password").hasClass("uk-form-success") &&
            $("#mail-password-again").hasClass("uk-form-success") &&
            $("#http-password").hasClass("uk-form-success") &&
            $("#http-password-again").hasClass("uk-form-success") &&
            $("#smtp-port").hasClass("uk-form-success") &&
            $("#pop3-port").hasClass("uk-form-success") &&
            $("#http-port").hasClass("uk-form-success") &&
            (routingType == "blind" ||
             (routingType == "location" && useGPS) ||
             ((routingType == "location" && !useGPS &&
               $("#longitude").hasClass("uk-form-success") &&
               $("#latitude").hasClass("uk-form-success"))))) {
            if ($("#reinstall-button").prop("disabled")) {
                $("#reinstall-button").prop("disabled", false);
            }
        } else {
            if (!$("#reinstall-button").prop("disabled")) {
                $("#reinstall-button").prop("disabled", true);
            }
        }
    };

    var locationKeyupHandler = function(id) {
        var handler = function() {
            if (isNaN($(id).val())) {
                Mixmesh.invalidInput(id);
            } else {
                Mixmesh.validInput(id);
            }
            toggleReinstallButton();
        };
        return handler;
    };
    
    var step4 = function(mailPassword, nym, smtpAddress, pop3Address,
                         httpAddress) {
        $("#meta-content").load(
            "/reinstall-4.html #content",
            function() {
                $("#email-address").val(nym + "@mixmesh.net");
                var ip_port = smtpAddress.split(":");
                $("#smtp-ip-address").val(ip_port[0]);
                $("#smtp-port").val(ip_port[1]);
                ip_port = pop3Address.split(":");
                $("#pop3-ip-address").val(ip_port[0]);
                $("#pop3-port").val(ip_port[1]);
                $("#mail-password").val(mailPassword);
            });
    };

    var step3 = function(mailPassword, nym, smtpAddress, pop3Address,
                         httpAddress, obscreteDir, pin, pinSalt) {
        $("#meta-content").load(
            "/reinstall-3.html #content",
            function() {
                // Next button
                $("#next-button").click(function() {
                    step4(mailPassword, nym, smtpAddress, pop3Address,
                          httpAddress);
                });

                // Select button
                UIkit.upload("#select-contacts", {
                    url: "/bootstrap/key/import",
                    name: "key-file",
                    multiple: false,
                    allow : "*.bin",
                    params: {
                        nym: nym,
                        "obscrete-dir": obscreteDir,
                        pin: pin,
                        "pin-salt": pinSalt
                    },
                    fail: function(reason) {
                        Mixmesh.showGenericDialog({
                            title: "Import failed",
                            content: "<p>" + reason + "</p>",
                            onok: function() {
                                Mixmesh.hideGenericDialog();
                            }
                        });
                    },
                    error: function(e) {
                        Mixmesh.showGenericDialog({
                            title: "Import failed",
                            content: "<p>" + Mixmesh.formatError(e.xhr) + "</p>",
                            onok: function() {
                                Mixmesh.hideGenericDialog();
                            }
                        });
                    },
                    completeAll: function(xhr) {
                        Mixmesh.showGenericDialog({
                            title: "Import succeeded",
                            content: "<p>You have imported " + xhr.responseText + " contacts.</p>",
                            onok: function() {
                                Mixmesh.hideGenericDialog();
                            }
                        });
                    }
                });
            });
    };

    var step2 = function(publicKey, secretKey) {
        $("#meta-content").load(
            "/reinstall-2.html #content",
            function() {
                $("#pin").keyup(function() {
                    if ($(this).val().length == 6 &&
                        /^\d+$/.test($(this).val())) {
                        Mixmesh.setClass(this, "uk-form-success",
                                         "uk-form-danger");
                    } else {
                        Mixmesh.setClass(this, "uk-form-danger",
                                         "uk-form-success");
                    }
                    toggleReinstallButton();
                });
                $("#mail-password")
                    .keyup(Mixmesh
                           .passwordKeyupHandler(
                               "#mail-password", toggleReinstallButton));
                $("#mail-password-again")
                    .keyup(Mixmesh
                           .passwordKeyupHandler(
                               "#mail-password", toggleReinstallButton));
                $("#http-password")
                    .keyup(Mixmesh.passwordKeyupHandler(
                        "#http-password", toggleReinstallButton));
                $("#http-password-again")
                    .keyup(Mixmesh
                           .passwordKeyupHandler(
                               "#http-password", toggleReinstallButton));
                $("#mail-password-lock")
                    .click(Mixmesh.passwordLockHandler("#mail-password"));
                $("#http-password-lock")
                    .click(Mixmesh.passwordLockHandler("#http-password"));
                $("#smtp-port")
                    .keyup(Mixmesh.portKeyupHandler("#smtp-port", toggleReinstallButton));
                $("#pop3-port")
                    .keyup(Mixmesh.portKeyupHandler("#pop3-port", toggleReinstallButton));
                $("#http-port")
                    .keyup(Mixmesh.portKeyupHandler("#http-port", toggleReinstallButton));
                $("#longitude").keyup(locationKeyupHandler("#longitude"));
                $("#latitude").keyup(locationKeyupHandler("#latitude"));
                $("#use-gps").click(function() {
                    $("#static-location").hide();
                    routingType = "location";
                    useGPS = true;
                    toggleReinstallButton();
                });
                $("#use-static-location").click(function() {
                    $("#static-location").show({
                        duration: 0,
                        complete: function() {
                            routingType = "location",
                            useGPS = false;
                            toggleReinstallButton();
                        }
                    });
                });
                $("#blind-routing")
                    .click(function() {
                        $("#static-location").hide();
                        routingType = "blind";
                        useGPS = false;
                        toggleReinstallButton();
                    });
                if (navigator.geolocation) {
                    $("#get-location-button").prop("disabled", false);
                    $("#get-location-button")
                        .click(function() {
                            var onSuccess = function(position) {
                                $("#longitude").val(position.coords.longitude);
                                (locationKeyupHandler("#longitude"))();
                                $("#latitude").val(position.coords.latitude);
                                (locationKeyupHandler("#latitude"))();
                                Mixmesh.unlockScreen(lockOwner);
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
                                        Mixmesh.unlockScreen(lockOwner);
                                        Mixmesh.hideGenericDialog();
                                    }
                                });
                            };
                            lockOwner = Mixmesh.lockScreen();
                            navigator.geolocation.getCurrentPosition(onSuccess, onError);
                            return false;
                        });
                }
                $("#reinstall-button").click(function() {
                    $("#reinstall-button").prop("disabled", true);
                    
                    if (routingType == "location" && !useGPS) {
                        longitude = Number($("#longitude").val());
                        latitude = Number($("#latitude").val());
                    }
                    
                    Mixmesh.post(
                        "/bootstrap/reinstall",
                        {                            
                            "public-key": publicKey,
                            "secret-key": secretKey,
                            "smtp-password": $("#mail-password").val(),
                            "pop3-password": $("#mail-password").val(),
                            "http-password": $("#http-password").val(),
                            "routing-type": routingType,
                            "use-gps": useGPS,
                            "longitude": longitude,
                            "latitude": latitude,
                            "smtp-port": parseInt($("#smtp-port").val()),
                            "pop3-port": parseInt($("#pop3-port").val()),
                            "http-port": parseInt($("#http-port").val()),
                            pin: $("#pin").val()
                        },
                        function(data, textStatus, _jqXHR) {
                            console.log(
                                "/reinstall (POST) succeeded");
                            console.log(data);

                            // Disable top-level navigation bar
                            $("#navbar-reinstall a")
                                .removeAttr("href");
                            $("#navbar-reinstall")
                                .removeClass("uk-active");
                            $("#navbar-install a").removeAttr("href");

                            step3($("#mail-password").val(),
                                  data.nym,
                                  data["smtp-address"],
                                  data["pop3-address"],
                                  data["http-address"],
                                  data["obscrete-dir"],
                                  data.pin,
                                  data["pin-salt"]);
                        },
                        function(jqXHR, textStatus, errorThrown) {
                            console.log("/reinstall (POST) failed");
                            console.log("textStatus: " + textStatus);
                            console.log("errorThrown: " + errorThrown);
                            Mixmesh.showGenericDialog({
                                title: "Reinstall failed",
                                content: "<p>" + Mixmesh.formatError(
                                    jqXHR, textStatus, errorThrown) + "</p>",
                                onok: function() {
                                    Mixmesh.hideGenericDialog();
                                }
                            });
                            $("#reinstall-button").prop("disabled", false);
                        });
                });
            })
    };

    var step1 = function() {
        var onScanFailure = function(error) {
	    console.warn(`QR error = ${error}`);
        }

        var html5QrcodeScanner = new Html5QrcodeScanner(
	    "reader", { fps: 10, qrbox: 400 }, /* verbose= */ true);

        var onScanSuccess = function(qrMessage) {
            html5QrcodeScanner.clear();
            console.log(qrMessage);
            var publicKey = qrMessage.substring(0, 180);
            var secretKey = qrMessage.substring(180);

            Mixmesh.showGenericDialog({
                title: "Key recognized",
                content: "<p>Provide passwords and ports.</p>",
                onok: function() {
                    Mixmesh.hideGenericDialog();
                }
            });

            step2(publicKey, secretKey);
        };

        html5QrcodeScanner.render(onScanSuccess, onScanFailure);
    };

    return {
        step1: step1
    }
})();

$(document).ready(function() {
    Reinstall.step1();
});

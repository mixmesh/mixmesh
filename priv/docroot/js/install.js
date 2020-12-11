var Install = (function() {
    var routingType = "location";
    var useGPS = true;
    var longitude = 0;
    var latitude = 0;
    var lockOwner;
    
    var toggleInstallButton = function() {
        if ($("#pin").hasClass("uk-form-success") &&
            $("#pseudonym").hasClass("uk-form-success") &&
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
            if ($("#install-button").prop("disabled")) {
                $("#install-button").prop("disabled", false);
            }
        } else {
            if (!$("#install-button").prop("disabled")) {
                $("#install-button").prop("disabled", true);
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
            toggleInstallButton();
        };
        return handler;
    };
    
    var step3 = function(nym, mailPassword, smtpAddress, pop3Address,
                         httpAddress) {
        $("#meta-content").load(
            "/install-3.html #content",
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

    var step2 = function(nym, mailPassword, httpPassword, smtpAddress,
                         pop3Address, httpAddress, publicKey, secretKey) {
        $("#meta-content").load(
            "/install-2.html #content",
            function() {
                var qr = new QRious({
                    element: $("#qrcode")[0],
                    size: 800,
                    value: publicKey + secretKey
                });
                $("#next-button").click(function() {
                    step3(nym, mailPassword, smtpAddress, pop3Address,
                          httpAddress);
                });
            })
    };

    var step1 = function() {
        $("#pin").keyup(function() {
            if ($(this).val().length == 6 && /^\d+$/.test($(this).val())) {
                Mixmesh.setClass(this, "uk-form-success", "uk-form-danger");
            } else {
                Mixmesh.setClass(this, "uk-form-danger", "uk-form-success");
            }
            toggleInstallButton();
        });
        $("#pseudonym").keyup(function() {
            if ($(this).val().length == 0) {
                Mixmesh.setClass(this, "uk-form-danger", "uk-form-success");
            } else {
                Mixmesh.setClass(this, "uk-form-success", "uk-form-danger");
            }
            toggleInstallButton();
        });
        $("#mail-password")
            .keyup(Mixmesh
                   .passwordKeyupHandler("#mail-password", toggleInstallButton))
        $("#mail-password-again")
            .keyup(Mixmesh
                   .passwordKeyupHandler("#mail-password", toggleInstallButton));
        $("#http-password")
            .keyup(Mixmesh
                   .passwordKeyupHandler("#http-password", toggleInstallButton));
        $("#http-password-again")
            .keyup(Mixmesh
                   .passwordKeyupHandler("#http-password", toggleInstallButton));
        $("#mail-password-lock")
            .click(Mixmesh.passwordLockHandler("#mail-password"));
        $("#http-password-lock")
            .click(Mixmesh.passwordLockHandler("#http-password"));
        $("#smtp-port")
            .keyup(Mixmesh.portKeyupHandler("#smtp-port", toggleInstallButton));
        $("#pop3-port")
            .keyup(Mixmesh.portKeyupHandler("#pop3-port", toggleInstallButton));
        $("#http-port")
            .keyup(Mixmesh.portKeyupHandler("#http-port", toggleInstallButton));
        $("#longitude").keyup(locationKeyupHandler("#longitude"));
        $("#latitude").keyup(locationKeyupHandler("#latitude"));
        $("#use-gps").click(function() {
            $("#static-location").hide();
            routingType = "location";
            useGPS = true;
            toggleInstallButton();
        });
        $("#use-static-location").click(function() {
            $("#static-location").show({
                duration: 0,
                complete: function() {
                    routingType = "location",
                    useGPS = false;
                    toggleInstallButton();
                }
            });
        });
        $("#blind-routing")
            .click(function() {
                $("#static-location").hide();
                routingType = "blind";
                useGPS = false;
                toggleInstallButton();
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
        $("#install-button").click(function() {
            $("#install-button").prop("disabled", true);

            if (routingType == "location" && !useGPS) {
                longitude = Number($("#longitude").val());
                latitude = Number($("#latitude").val());
            }
            
            Mixmesh.post(
                "/bootstrap/install",
                {
                    nym: $("#pseudonym").val(),
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
                    console.log("/install (POST) succeeded");
                    console.log(data);

                    // Disable top-level navigation bar
                    $("#navbar-install a").removeAttr("href");
                    $("#navbar-install").removeClass("uk-active");
                    $("#navbar-reinstall a").removeAttr("href");

                    step2($("#pseudonym").val(),
                          $("#mail-password").val(),
                          $("#http-password").val(),
                          data["smtp-address"],
                          data["pop3-address"],
                          data["http-address"],
                          data["public-key"],
                          data["secret-key"]);
                },
                function(jqXHR, textStatus, errorThrown) {
                    console.log("/install (POST) failed");
                    console.log("textStatus: " + textStatus);
                    console.log("errorThrown: " + errorThrown);
                    Mixmesh.showGenericDialog({
                        title: "Install failed",
                        content: "<p>" + Mixmesh.formatError(
                            jqXHR, textStatus, errorThrown) + "</p>",
                        onok: function() {
                            Mixmesh.hideGenericDialog();
                        }
                    });
                    $("#install-button").prop("disabled", false);
                })
        })
    };

    return {
        step1: step1
    };
})();

$(document).ready(function() {
    Install.step1();
});

var Reinstall = (function() {
    var validPassword =
        function(id) {
            Reinstall.setClass(id, "uk-form-success", "uk-form-danger");
            Reinstall.setClass(id + "-again", "uk-form-success",
                               "uk-form-danger");
        };
    
    var invalidPassword =
        function(id) {
            Reinstall.setClass(id, "uk-form-danger", "uk-form-success");
            Reinstall.setClass(id + "-again",  "uk-form-danger",
                               "uk-form-success");
        };
    
    return {
        setClass: function(id, newClass, oldClass) {
            if (!$(id).hasClass(newClass)) {
                $(id).removeClass(oldClass);
                $(id).addClass(newClass);
            }
        },
        toggleReinstallButton: function() {
            if ($("#mail-password").hasClass("uk-form-success") &&
                $("#http-password").hasClass("uk-form-success")) {
                if ($("#reinstall-button").prop('disabled')) {
                    $("#reinstall-button").prop('disabled', false);
                }
            } else {
                if (!$("#reinstall-button").prop('disabled')) {
                    $("#reinstall-button").prop('disabled', true);
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
                    Reinstall.toggleReinstallButton();
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
        },
        step2: function(publicKey, secretKey) {
            $("#meta-content").load(
                "/reinstall-2.html #content",
                function() {
                    $("#mail-password")
                        .keyup(Reinstall
                               .passwordKeyupHandler("#mail-password"));
                    $("#mail-password-again")
                        .keyup(Reinstall
                               .passwordKeyupHandler("#mail-password"));
                    $("#http-password")
                        .keyup(Reinstall
                               .passwordKeyupHandler("#http-password"));
                    $("#http-password-again")
                        .keyup(Reinstall
                               .passwordKeyupHandler("#http-password"));    
                    $("#mail-password-lock")
                        .click(Reinstall
                               .passwordLockHandler("#mail-password"));
                    $("#http-password-lock")
                        .click(Reinstall
                               .passwordLockHandler("#http-password"));
                    $("#reinstall-button").click(
                        function() {
                            $("#reinstall-button").prop('disabled', true);
                            Mixmesh.post(
                                "/dj/system/reinstall",
                                {
                                    "public-key": publicKey,
                                    "secret-key": secretKey,
                                    "smtp-password": $("#mail-password").val(),
                                    "pop3-password": $("#mail-password").val(),
                                    "http-password": $("#http-password").val()
                                },
                                function(data, textStatus, _jqXHR) {
                                    console.log(
                                        "/dj/reinstall (POST) succeeded");
                                    console.log(data);
                                    
                                    // Disable top-level navigation bar
                                    $("#navbar-reinstall a")
                                        .removeAttr("href");    
                                    $("#navbar-reinstall")
                                        .removeClass("uk-active");
                                    $("#navbar-wipe a").removeAttr("href");    
                                    
                                    Reinstall.step3(
                                        $("#mail-password").val(),
                                        data.nym,
                                        data["smtp-address"],
                                        data["pop3-address"],
                                        data["http-address"],
                                        data["obscrete-dir"],
                                        data.pin,
                                        data["pin-salt"]);
                                },
                                function(jqXHR, textStatus, errorThrown) {
                                    console.log("/dj/reinstall (POST) failed");
                                    console.log("textStatus: " + textStatus);
                                    console.log("errorThrown: " + errorThrown);

                                    // Error dialog
                                    $("#generic-dialog-title")
                                        .text("Reinstall failed").show();
                                    $("#generic-dialog-content")
                                        .html("<p>Internal error: " +
                                              jqXHR.responseText + "</p>");
                                    UIkit.modal("#generic-dialog").show();
                                    $("#reinstall-button")
                                        .prop('disabled', false);
                                });
                        });
                })
        },
        step3: function(mailPassword, nym, smtpAddress, pop3Address,
                        httpAddress, obscreteDir, pin, pinSalt) {
            $("#meta-content").load(
                "/reinstall-3.html #content",
                function() {
                    // Next button
                    $("#next-button").click(function() {
                        Reinstall
                            .step4(mailPassword, nym, smtpAddress, pop3Address,
                                   httpAddress);
                    });

                    // Select button
                    UIkit.upload("#select-contacts", {
                        url: "/dj/key/import",
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
                            $("#generic-dialog-title")
                                .text("Import failed").show();
                            $("#generic-dialog-content")
                                .html("<p>" + reason + "</p>");
                            UIkit.modal("#generic-dialog").show();
                        },
                        error: function(e) {
                            $("#generic-dialog-title")
                                .text("Import failed").show();
                            $("#generic-dialog-content")
                                .html("<p>" + e.xhr.response + "</p>");
                            UIkit.modal("#generic-dialog").show();
                        },
                        completeAll: function(xhr) {
                            $("#generic-dialog-title")
                                .text("Import succeeded").show();
                            $("#generic-dialog-content")
                                .empty()
                                .html("<p>You have imported " +
                                      xhr.responseText +
                                      " public keys.</p>");
                            UIkit.modal("#generic-dialog").show();
                        }
                    });
                });
        },
        step4: function(mailPassword, nym, smtpAddress, pop3Address,
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
        }
    }
})();

$(document).ready(function() {
    setTimeout(function() {
        // Hardwired for now
        Reinstall.step2("BWFsaWNlBbqW75jjJ0aPtaq1zGPObUc7ZQ2WIwIRbX2bkVyOkeIkAC9Hg0oc+J7BD\/RG04TDvd1fETcpmJpyvV8QyeKJ3B3BMHi+LPWSRY60yX1XoA\/1A1iuIxTnt22Q68iXyMMlZvA+ivmNxJlsqN3PB2KOch45KkNzi9Hez9u7KTZBhp3d", "BWFsaWNlgMwhWxEO5Ovn0OpNnN62Mu9nvL7Zn1mzlgSBkfC2zZQII\/otb+1jPqLMCDQlFKqNEXGy\/N1PUhotV3w7JBitwsZSUeGfVi2gLJFEkrZ6tGjrUoN3eB65JIzpfQirlLX6oCO5Ab1t4rOmD4BsHvA+lYBbYw3QihArIGqcTyNrbiC1BbqW75jjJ0aPtaq1zGPObUc7ZQ2WIwIRbX2bkVyOkeIkAC9Hg0oc+J7BD\/RG04TDvd1fETcpmJpyvV8QyeKJ3B3BMHi+LPWSRY60yX1XoA\/1A1iuIxTnt22Q68iXyMMlZvA+ivmNxJlsqN3PB2KOch45KkNzi9Hez9u7KTZBhp3d");
    }, 8000);
});

$("bajs").ready(function() {    
    $("#reinstall-button").click(
        function() {
            $("#reinstall-button").prop('disabled', true);

            // Remember these
            Reinstall.pseudonym = $("#pseudonym").val();
            Reinstall.mailPassword = $("#mail-password").val();

            Mixmesh.post(
                "/dj/system/reinstall",
                {
                    nym: $("#pseudonym").val(),
                    "smtp-password": $("#mail-password").val(),
                    "pop3-password": $("#mail-password").val(),
                    "http-password": $("#http-password").val()
                },
                function(data, textStatus, _jqXHR) {
                    console.log("/dj/reinstall (POST) succeeded");
                    console.log(data);

                    // Disable top-level navigation bar
                    $("#navbar-reinstall a").removeAttr("href");    
                    $("#navbar-reinstall").removeClass("uk-active");
                    $("#navbar-wipe a").removeAttr("href");    
                    
                    // Load step 2
                    $("#meta-content").load(
                        "/reinstall-2.html #content",
                        function() {
                            // To avoid flicker (see below)
                            $("#meta-content").hide();
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
                        });
                },
                function(_jqXHR, textStatus, errorThrown) {
                    console.log("/dj/reinstall (POST) failed");
                    console.log("textStatus: " + textStatus);
                    console.log("errorThrown: " + errorThrown);
                });
        });
});

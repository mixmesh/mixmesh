var Reinstall = (function() {
    var toggleReinstallButton = function() {
        if ($("#mail-password").hasClass("uk-form-success") &&
            $("#mail-password-again").hasClass("uk-form-success") &&
            $("#http-password").hasClass("uk-form-success") &&
            $("#http-password-again").hasClass("uk-form-success")) {
            if ($("#reinstall-button").prop('disabled')) {
                $("#reinstall-button").prop('disabled', false);
            }
        } else {
            if (!$("#reinstall-button").prop('disabled')) {
                $("#reinstall-button").prop('disabled', true);
            }
        }
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
                        $("#generic-dialog-title").text("Import failed").show();
                        $("#generic-dialog-content")
                            .html("<p>" + reason + "</p>");
                        UIkit.modal("#generic-dialog").show();
                    },
                    error: function(e) {
                        $("#generic-dialog-title").text("Import failed").show();
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
    };
    
    var step2 = function(publicKey, secretKey) {
        $("#meta-content").load(
            "/reinstall-2.html #content",
            function() {
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
                $("#reinstall-button").click(function() {
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
    };
    
    var step1 = function() {
        setTimeout(function() {
            // Hardwired for now
            step2("BWFsaWNlBbqW75jjJ0aPtaq1zGPObUc7ZQ2WIwIRbX2bkVyOkeIkAC9Hg0oc+J7BD\/RG04TDvd1fETcpmJpyvV8QyeKJ3B3BMHi+LPWSRY60yX1XoA\/1A1iuIxTnt22Q68iXyMMlZvA+ivmNxJlsqN3PB2KOch45KkNzi9Hez9u7KTZBhp3d", "BWFsaWNlgMwhWxEO5Ovn0OpNnN62Mu9nvL7Zn1mzlgSBkfC2zZQII\/otb+1jPqLMCDQlFKqNEXGy\/N1PUhotV3w7JBitwsZSUeGfVi2gLJFEkrZ6tGjrUoN3eB65JIzpfQirlLX6oCO5Ab1t4rOmD4BsHvA+lYBbYw3QihArIGqcTyNrbiC1BbqW75jjJ0aPtaq1zGPObUc7ZQ2WIwIRbX2bkVyOkeIkAC9Hg0oc+J7BD\/RG04TDvd1fETcpmJpyvV8QyeKJ3B3BMHi+LPWSRY60yX1XoA\/1A1iuIxTnt22Q68iXyMMlZvA+ivmNxJlsqN3PB2KOch45KkNzi9Hez9u7KTZBhp3d");
        }, 8000);
    };
    
    return {
        step1: step1
    }
})();

$(document).ready(function() {
    Reinstall.step1();
});

var Wipe = (function() {
    var toggleWipeButton = function() {
        if ($("#pseudonym").hasClass("uk-form-success") &&
            $("#mail-password").hasClass("uk-form-success") &&
            $("#mail-password-again").hasClass("uk-form-success") &&
            $("#http-password").hasClass("uk-form-success") &&
            $("#http-password-again").hasClass("uk-form-success")) {
            if ($("#wipe-button").prop('disabled')) {
                $("#wipe-button").prop('disabled', false);
            }
        } else {
            if (!$("#wipe-button").prop('disabled')) {
                $("#wipe-button").prop('disabled', true);
            }
        }
    };
        
    var step3 = function(nym, mailPassword, smtpAddress, pop3Address,
                         httpAddress) {
        $("#meta-content").load(
            "/wipe-3.html #content",
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
            "/wipe-2.html #content",
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
        $("#pseudonym").keyup(function() {
            if ($(this).val().length == 0) {
                Mixmesh.setClass(this, "uk-form-danger", "uk-form-success");
            } else {
                Mixmesh.setClass(this, "uk-form-success", "uk-form-danger");
            }
            toggleWipeButton();
        });    
        $("#mail-password")
            .keyup(Mixmesh
                   .passwordKeyupHandler("#mail-password", toggleWipeButton))
        $("#mail-password-again")
            .keyup(Mixmesh
                   .passwordKeyupHandler("#mail-password", toggleWipeButton));
        $("#http-password")
            .keyup(Mixmesh
                   .passwordKeyupHandler("#http-password", toggleWipeButton));
        $("#http-password-again")
            .keyup(Mixmesh
                   .passwordKeyupHandler("#http-password", toggleWipeButton));    
        $("#mail-password-lock")
            .click(Mixmesh.passwordLockHandler("#mail-password"));
        $("#http-password-lock")
            .click(Mixmesh.passwordLockHandler("#http-password"));
        $("#wipe-button").click(function() {
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
                    
                    // Disable top-level navigation bar
                    $("#navbar-wipe a").removeAttr("href");    
                    $("#navbar-wipe").removeClass("uk-active");
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
                    console.log("/dj/wipe (POST) failed");
                    console.log("textStatus: " + textStatus);
                    console.log("errorThrown: " + errorThrown);
                    Mixmesh.showGenericDialog({
                        title: "Wipe failed",
                        content: "<p>" + Mixmesh.formatError(jqXHR, textStatus, errorThrown) + "</p>",
                        onok: function() {
                            Mixmesh.hideGenericDialog();
                        }
                    });
                    $("#wipe-button").prop('disabled', false);
                })
        })
    };
    
    return {
        step1: step1
    };
})();

$(document).ready(function() {
    Wipe.step1();
});

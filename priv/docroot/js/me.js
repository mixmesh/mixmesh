$(document).ready(function() {
    Mixmesh.get(
        "/seconds-since-initialization",
        function(secondsSinceInitialization, status, xhr) {
            if (status == "success") {
                Mixmesh.post(
                    "/get-config",
                    {
                        player: {
                            "sync-server": {
                                "public-key": true,
                                "secret-key": true
                            }
                        }
                    },
                    function(data, textStatus, _jqXHR) {
                        console.log("/get-config (POST) succeeded");
                        console.log(data.player["sync-server"]["public-key"]);

                        if (secondsSinceInitialization < 3600) {
                            $("#title").text("Secret key");
                            $("#description")
                                .html("This is your secret key. Take a screenshot of the key now before it is too late. The key will go away in " + Mixmesh.formatAmount(Math.round((3600 - secondsSinceInitialization) / 60), "minute") + ".</p>");
                            var qr = new QRious({
                                element: $("#qrcode")[0],
                                size: 800,
                                value: data.player["sync-server"]["public-key"] +
                                    data.player["sync-server"]["secret-key"]
                            });
                        } else {
                            $("#title").text("Public key");
                            $("#description")
                                .html("This is your public key. Show this key to anyone you want to communicate with and ask them to add it to their list of contacts.");
                            var qr = new QRious({
                                element: $("#qrcode")[0],
                                size: 800,
                                value: data.player["sync-server"]["public-key"]
                            });
                        }

                    },
                    function(jqXHR, textStatus, errorThrown) {
                        console.log("/get-config (POST) failed");
                        console.log("textStatus: " + textStatus);
                        console.log("errorThrown: " + errorThrown);
                        Mixmesh.showGenericDialog({
                            title: "Could not get key",
                            content: "<p>" + Mixmesh.formatError(
                                jqXHR, textStatus, errorThrown) + "</p>",
                            onok: function() {
                                Mixmesh.hideGenericDialog();
                            }
                        });
                    });
            } else {
                console.log("/seconds-since-initialization (GET) failed");
                console.log(secondsSinceInitialization);
                Mixmesh.showGenericDialog({
                    title: "System not responsive",
                    content: "<p>" + Mixmesh.formatError(xhr) + "</p>",
                    onok: function() {
                        Mixmesh.hideGenericDialog();
                    }
                });
            }
        });

});

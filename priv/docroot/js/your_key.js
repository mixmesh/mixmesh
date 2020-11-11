$(document).ready(function() {    
    Mixmesh.post(
        "/dj/get-config",
        {
            player: {
                spiridon: {
                    "public-key": true,
                    "secret-key": true
                }
            }
        },
        function(data, textStatus, _jqXHR) {
            console.log("/dj/get-config (POST) succeeded");
            console.log(data.player.spiridon["public-key"]);
            var qr = new QRious({
                element: $("#qrcode")[0],
                size: 800,
                value: data.player.spiridon["public-key"]
            });
        },
        function(jqXHR, textStatus, errorThrown) {
            console.log("/dj/get-config (POST) failed");
            console.log("textStatus: " + textStatus);
            console.log("errorThrown: " + errorThrown);
            Mixmesh.showGenericDialog({
                title: "Could not get key",
                content: "<p>" + Mixmesh.formatError(jqXHR, textStatus, errorThrown) + "</p>",
                onok: function() {
                    Mixmesh.hideGenericDialog();
                }
            });                    
        });
});

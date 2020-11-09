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
            $("#qrcode").hide(); // To avoid flicker (see below)
            new QRCode($("#qrcode").get(0), {
	        text: data.player.spiridon["public-key"] +
                    data.player.spiridon["secret-key"],
	        width: 800,
	        height: 800,
	        colorDark : "#000000",
	        colorLight : "#ffffff",
	        correctLevel : QRCode.CorrectLevel.H
            });
            // To avoid flicker (see above)
            setTimeout(function() {
                $("#qrcode").show();
            }, 10);
        },
        function(_jqXHR, textStatus, errorThrown) {
            console.log("/dj/get-config (POST) failed");
            console.log("textStatus: " + textStatus);
            console.log("errorThrown: " + errorThrown);
        });
});


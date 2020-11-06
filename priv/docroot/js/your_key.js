$(document).ready(function() {
    Mixmesh.setHeight("#content", ["#navigation"]);
    
    // Show key
    Mixmesh.post(
        "/dj/get-config",
        {"player": {"spiridon": {"public-key": true}}},
        function(data, textStatus, _jqXHR) {
            console.log("/dj/get-config (POST) succeeded");
            console.log(data.player.spiridon["public-key"]);
            new QRCode($("#content").get(0), {
	        text: data.player.spiridon["public-key"],
	        width: 800,
	        height: 800,
	        colorDark : "#000000",
	        colorLight : "#ffffff",
	        correctLevel : QRCode.CorrectLevel.H
            });
        },
        function(_jqXHR, textStatus, errorThrown) {
            console.log("/dj/get-config (POST) failed");
            console.log("textStatus: " + textStatus);
            console.log("errorThrown: " + errorThrown);
        });
});


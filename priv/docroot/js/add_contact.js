$(document).ready(function() {
    var onScanFailure = function(error) {
	console.warn(`QR error = ${error}`);
    }
    
    var html5QrcodeScanner = new Html5QrcodeScanner(
	"reader", { fps: 10, qrbox: 500 }, /* verbose= */ true);
    
    var onScanSuccess = function(qrMessage) {
        console.log(qrMessage);
        html5QrcodeScanner.clear();
        Mixmesh.put(
            "/dj/key",
            qrMessage,
            function(nym, textStatus, _jqXHR) {
                console.log("/dj/key (PUT) succeeded");
                console.log("Contact have been added");
                Mixmesh.showGenericDialog({
                    title: "Contact recognized",
                    content: "You have added " + nym + " to your list of contacts.",
                    onok: function() {
                        Mixmesh.hideGenericDialog();
                    }
                });
                html5QrcodeScanner.render(onScanSuccess, onScanFailure);
            },
            function(jqXHR, textStatus, errorThrown) {
                console.log("/dj/key (PUT) failed");
                console.log("textStatus: " + textStatus);
                console.log("errorThrown: " + errorThrown);
                Mixmesh.showGenericDialog({
                    title: "Contact not recognized",
                    content: "<p>" + Mixmesh.formatError(jqXHR, textStatus, errorThrown) + "</p>",
                    onok: function() {
                        Mixmesh.hideGenericDialog();
                        UIkit.modal("#generic-dialog").hide();
                    }
                });
                html5QrcodeScanner.render(onScanSuccess, onScanFailure);
            });
    };
    
    html5QrcodeScanner.render(onScanSuccess, onScanFailure);
});

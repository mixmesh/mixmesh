$(document).ready(function() {
    var onScanSuccess = function(qrMessage) {
        html5QrcodeScanner.clear();
        Mixmesh.showGenericDialog({
            title: "Contact recognized",
            content: "You have added a new contact to your list of contacts.",
            onok: function() {
                Mixmesh.hideGenericDialog();
            }
        });
    };
    
    var onScanFailure = function(error) {
	console.warn(`QR error = ${error}`);
    }
    
    var html5QrcodeScanner = new Html5QrcodeScanner(
	"reader", { fps: 10, qrbox: 500 }, /* verbose= */ true);
    html5QrcodeScanner.render(onScanSuccess, onScanFailure);
});

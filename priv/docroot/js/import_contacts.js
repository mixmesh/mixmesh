$(document).ready(function() {
    UIkit.upload("#select-contacts", {
        url: "/dj/key/import",
        name: "key-file",
        multiple: false,
        allow : "*.bin",
        fail: function(reason) {
            Mixmesh.showGenericDialog({
                title: "Import failed",
                content: "<p>" + reason + "</p>",
                onok: function() {
                    Mixmesh.hideGenericDialog();
                    UIkit.modal("#generic-dialog").hide();
                }
            });
        },
        error: function(e) {
            Mixmesh.showGenericDialog({
                title: "Import failed",
                content: "<p>" + Mixmesh.formatXHRError(e.xhr) + "</p>",
                onok: function() {
                    Mixmesh.hideGenericDialog();
                }
            });
        },
        completeAll: function(xhr) {
            Mixmesh.showGenericDialog({
                title: "Import succeeded",
                content: "<p>You have imported " + xhr.responseText + " contacts.</p>",
                onok: function() {
                    Mixmesh.hideGenericDialog();
                }
            });
        }
    });
});

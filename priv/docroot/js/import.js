$(document).ready(function() {
    UIkit.upload("#select-contacts", {
        url: "/key/import",
        name: "key-file",
        multiple: false,
        allow : "*.bin",
        fail: function(reason) {
            Mixmesh.showGenericDialog({
                title: "Import failed",
                content: "<p>" + reason + "</p>",
                onok: function() {
                    Mixmesh.hideGenericDialog();
                }
            });
        },
        error: function(e) {
            Mixmesh.showGenericDialog({
                title: "Import failed",
                content: "<p>" + Mixmesh.formatError(e.xhr) + "</p>",
                onok: function() {
                    Mixmesh.hideGenericDialog();
                }
            });
        },
        completeAll: function(xhr) {
            Mixmesh.showGenericDialog({
                title: "Import succeeded",
                content: "<p>You have imported " + Mixmesh.formatAmount(xhr.responseText, "contact") + ".</p>",
                onok: function() {
                    Mixmesh.hideGenericDialog();
                }
            });
        }
    });
});

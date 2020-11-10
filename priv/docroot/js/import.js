$(document).ready(function() {
    UIkit.upload("#select-contacts", {
        url: "/dj/key/import",
        name: "key-file",
        multiple: false,
        allow : "*.bin",
        fail: function(reason) {
            $("#generic-dialog-title").text("Import failed").show();
            $("#generic-dialog-content").html("<p>" + reason + "</p>");
            UIkit.modal("#generic-dialog").show();
        },
        error: function(e) {
            $("#generic-dialog-title").text("Import failed").show();
            $("#generic-dialog-content").html("<p>" + e.xhr.response + "</p>");
            UIkit.modal("#generic-dialog").show();
        },
        completeAll: function(xhr) {
            $("#generic-dialog-title").text("Import succeeded").show();
            $("#generic-dialog-content")
                .html("<p>You have imported " + xhr.responseText +
                      " public keys.</p>");
            UIkit.modal("#generic-dialog").show();
        }
    });
});

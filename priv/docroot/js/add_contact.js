$(document).ready(function() {
    setTimeout(function() {
        Mixmesh.showGenericDialog({
            title: "Contact recognized",
            content: "You have added a new contact to your list of contacts.",
            onok: function() {
                Mixmesh.hideGenericDialog();
            }
        });
    }, 4000);
});

$(document).ready(function() {
    Mixmesh.setHeight("#content", ["#navigation"]);
    setTimeout(function() {
        console.log("bajs");
        UIkit.modal("#key-added-dialog").show();
    }, 4000);
});

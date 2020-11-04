var ListKeys = (function() {
    var privateVar = '';
    
    function privateMethod () {
        // ...
    }
    
    return {
        deleteKey: function(deleteKeyButton, nym) {
            Mixmesh.post(
                "/dj/key/delete", [nym],
                function(data, textStatus, _jqXHR) {
                    console.log("/dj/key/delete (POST) succeeded");
                    console.log(nym + " has been deleted");
                    console.log("data: " + data);
                    console.log("textStatus: " + textStatus);
                    $(deleteKeyButton).closest("tr").remove();
                },
                function(_jqXHR, textStatus, errorThrown) {
                    console.log("/dj/key/delete (POST) failed");
                    console.log("textStatus: " + textStatus);
                    console.log("errorThrown: " + errorThrown);
                });
        },
        truncate: function(fullStr, strLen, separator) {
            if (fullStr.length <= strLen) {
                return fullStr;
            }
            separator = separator || '...';
            var sepLen = separator.length;
            var charsToShow = strLen - sepLen;
            var frontChars = Math.ceil(charsToShow/2);
            var backChars = Math.floor(charsToShow/2);
            return fullStr.substr(0, frontChars) +
                separator + fullStr.substr(fullStr.length - backChars);
        }
    };
})();

$(document).ready(function() {
    Mixmesh.setHeight("#key-table",
                      ["#navigation", "#search", "#key-table-buttons"]);
    Mixmesh.get(
        "/dj/key",
        function(data, status) {
            if (status == "success") {
                $.each(data, function(_index, key) {
                    var row =
                        ml("tr", {},
                           [ml("td", {},
                               ml("input", {class: "uk-checkbox",
                                            type: "checkbox"})),
                            ml("td", {}, key.nym),
                            ml("td", {class: "uk-table-link"},
                               ml("a", {class: "uk-link-reset", href: "#0"},
                                  ListKeys.truncate(
                                      key["public-key"], 32, "..."))),,
                            ml("td", {},
                               ml("a", {
                                   href: "#0", class: "uk-icon-button",
                                   "uk-icon": "trash",
                                   onclick: function() {
                                       ListKeys.deleteKey(this, key.nym);
                                   }}))]);

                    jml.render($("#key-table-body"), row);


                    //$("#key-table-body").append(row);
                });
            } else {
                console.log("/dj/key (GET) failed");
                console.log(data);
            }
        });
});

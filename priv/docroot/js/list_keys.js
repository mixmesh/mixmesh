var ListKeys = (function() {
    var privateVar = '';
    
    function privateMethod () {
        // ...
    }
    
    return {
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
    $.get("/dj/key",
          function(keys, status) {
              if (status == "success") {
                  $.each(keys, function(_index, key) {
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
                                 ml("span", {"uk-icon": "trash"}))]);
                      //console.log(key);
                      $("#key-table-body").append(row);
                  });
              } else {
                  console.log("/dj/key (GET) failed");
              }
          });
});

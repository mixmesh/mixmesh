var ListKeys = (function() {
    var privateVar = '';
    
    function privateMethod () {
        // ...
    }

    return {
        truncate: function(fullStr, strLen, separator) {
            if (fullStr.length <= strLen) return fullStr;

            separator = separator || '...';

            var sepLen = separator.length,
                charsToShow = strLen - sepLen,
                frontChars = Math.ceil(charsToShow/2),
                backChars = Math.floor(charsToShow/2);

            return fullStr.substr(0, frontChars) +
                separator + fullStr.substr(fullStr.length - backChars);
        }
    };
})();

$(document).ready(function() {
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
                      console.log(key);
                      $("#tbody-keys").append(row);
                  });
              } else {
                  console.log("/dj/key (GET) failed");
              }
          });
});

var Mixmesh = (function() {
    var privateVar = '';

    function privateMethod () {
        // ...
    }

    return { // public interface
        truncateString: function(fullStr, strLen, separator) {
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

/*
$(document).ready(function(){
    $("button").click(function(){
        $(this).hide();
    });

    var overlay =
        ml("table", {},
           [ml("tr", {valign: "top"},
               [ml("th", {}, "bajs"),
                ml("th", {}, "bajs2")]),
            ml("tr", {valign: "top"},
               [ml("td", {}, "foo"),
                ml("td", {}, "bar")])]);

    $("#aa").append(overlay);
});
*/

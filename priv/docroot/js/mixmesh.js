var Mixmesh = (function() {
    var privateVar = '';
    
    function privateMethod () {
        // ...
    }
    return {
        setHeight: function(targetId, siblingIds) {
            var targetHeight = window.innerHeight;
            for (i = 0; i < siblingIds.length; i++) {
                targetHeight -= $(siblingIds[i]).outerHeight(true);
            }
            $(targetId).outerHeight(targetHeight.toString() + "px", true);
        },
        get: function(url, success) {
            $.get(url, null, success);
        },
        post: function(url, data, success, error) {
            $.ajax(url, {
                data: JSON.stringify(data),
                contentType: "application/json",
                type: "POST",
                success: success,
                error: error
            });
        }
    };
})();

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
        }
    };
})();

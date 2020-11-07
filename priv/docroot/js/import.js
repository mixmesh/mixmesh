$(document).ready(function() {
    Mixmesh.setHeight("#content", ["#navigation"]);

    UIkit.upload(".js-upload", {
        url: "/dj/key/import",
        multiple: false,
        beforeSend: function (environment) {
            console.log('beforeSend', arguments);
            // The environment object can still be modified here. 
            // var {data, method, headers, xhr, responseType} = environment;
        },
        beforeAll: function () {
            console.log('beforeAll', arguments);
        },
        load: function () {
            console.log('load', arguments);
        },
        error: function () {
            console.log('error', arguments);
        },
        complete: function () {
            console.log('complete', arguments);
        },
        loadStart: function (e) {
            console.log('loadStart', arguments);
        },
        progress: function (e) {
            console.log('progress', arguments);
        },
        loadEnd: function (e) {
            console.log('loadEnd', arguments);
        },        
        completeAll: function () {
            console.log('completeAll', arguments);
        }
    });
});

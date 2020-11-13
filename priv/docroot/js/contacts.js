var Contacts = (function() {
    var adornRows = function() {
        // Add event handlers to all checkboxes
        $("#key-table-body tr td :checkbox").click(function() {
            if ($(this).prop("checked")) {
                $("#delete-selected-button").prop("disabled", false);
                $("#export-selected-button").prop("disabled", false);
            } else {
                if ($("#key-table-body tr td :checkbox:checked")
                    .length == 0) {
                    $("#delete-selected-button")
                        .prop("disabled", true);
                    $("#export-selected-button")
                        .prop("disabled", true);
                }
            }
        });
    };

    var createRow = function(key) {
        var row =
            ml("tr", {},
               [ml("td", {},
                   ml("input", {class: "uk-checkbox", type: "checkbox"})),
                ml("td", {}, key.nym),
                ml("td", {class: "uk-table-link"},
                   ml("a", {href: "#show-key",
                            "uk-toggle": "",
                            class: "uk-link-reset",
                            "data-nym": key.nym,
                            "data-public-key": key["public-key"],
                            onclick: function(event) {
                                Contacts.showKey(event);
	                    }},
                      Contacts.truncate(
                          key["public-key"], 16, "..."))),,
                ml("td", {},
                   ml("span", {
                       class: "clickable uk-align-right",
                       "uk-icon": "trash",
                       onclick: function(event) {
                           Contacts.deleteKey(event, key.nym);
	               }}))]);
        return row;
    };

    return {
        deleteKey: function(event, nym) {
            Mixmesh.post(
                "/key/delete",
                [nym],
                function(data, textStatus, _jqXHR) {
                    console.log("/key/delete (POST) succeeded");
                    console.log(nym + " has been deleted");
                    $(event.target).closest("tr").remove();
                },
                function(jqXHR, textStatus, errorThrown) {
                    console.log("/key/delete (POST) failed");
                    console.log("textStatus: " + textStatus);
                    console.log("errorThrown: " + errorThrown);
                    Mixmesh.showGenericDialog({
                        title: "Contact could not be deleted",
                        content: "<p>" + Mixmesh.formatError(
                            jqXHR, textStatus, errorThrown) + "</p>",
                        onok: function() {
                            Mixmesh.hideGenericDialog();
                        }
                    });
                });
        },
        showKey: function(event) {
            var qr = new QRious({
                element: $("#qrcode")[0],
                size: 800,
                value: $(event.target).attr("data-public-key")
            });
        },
        refreshRows: function() {
            Mixmesh.get(
                "/key",
                function(data, status) {
                    if (status == "success") {
                        // Toggle buttons
                        $("#delete-selected-button").prop("disabled", true);
                        $("#export-selected-button").prop("disabled", true);
                        $("#export-all-button").prop(
                            "disabled", data.length == 0);

                        // Create key rows
                        $.each(data, function(_index, key) {
                            var row = createRow(key);
                            render($("#key-table-body")[0], row);
                        });
                        adornRows();
                    } else {
                        console.log("/key (GET) failed");
                        console.log(data);
                    }
                });
        },
        filterRows: function(subStringNym) {
            Mixmesh.post(
                "/key/filter",
                [subStringNym],
                function(data, textStatus, _jqXHR) {
                    console.log("/key/filter (POST) succeeded");
                    console.log(data + " keys were found");

                    // Toggle buttons
                    $("#delete-selected-button").prop("disabled", true);
                    $("#export-selected-button").prop("disabled", true);
                    $("#export-all-button").prop("disabled", data.length == 0);

                    // Remove all key rows
                    $("#key-table-body").empty();

                    // Create key rows
                    $.each(data, function(_index, key) {
                        var row = createRow(key);
                        render($("#key-table-body")[0], row);
                    });
                    adornRows();
                },
                function(jqXHR, textStatus, errorThrown) {
                    console.log("/key/filter (POST) failed");
                    console.log("textStatus: " + textStatus);
                    console.log("errorThrown: " + errorThrown);
                    Mixmesh.showGenericDialog({
                        title: "Filter not working",
                        content: "<p>" + Mixmesh.formatError(
                            jqXHR, textStatus, errorThrown) + "</p>",
                        onok: function() {
                            Mixmesh.hideGenericDialog();
                        }
                    });
                });
        },
        truncate: function(fullStr, strLen, separator) {
            if (fullStr.length <= strLen) {
                return fullStr;
            }
            separator = separator || "...";
            var sepLen = separator.length;
            var charsToShow = strLen - sepLen;
            var frontChars = Math.ceil(charsToShow / 2);
            var backChars = Math.floor(charsToShow / 2);
            return fullStr.substr(0, frontChars) +
                separator + fullStr.substr(fullStr.length - backChars);
        }
    };
})();

$(document).ready(function() {
    Mixmesh.setHeight("#key-table",
                      ["#navigation", "#filter", "#key-table-buttons"]);

    // Add handler to filter input
    $("#filter-input").keyup(function() {
        if ($(this).val().length == 0) {
            Contacts.refreshRows();
        } else {
            Contacts.filterRows($(this).val());
        }
    });

    // Select-all checkboxes
    $("#select-all").click(function() {
        var checkedStatus = this.checked;
        $("#delete-selected-button").prop("disabled", !checkedStatus);
        $("#export-selected-button").prop("disabled", !checkedStatus);
        $("#key-table-body tr").find("td:first :checkbox").each(function() {
            $(this).prop("checked", checkedStatus);
        });
    });

    // Add handler to delete-selected button
    $("#delete-selected-button").click(function() {
        var selectedRows = [];
        var selectedNyms = [];

        // Collect selected keys
        $("#key-table-body tr").each(function() {
            if ($(this).find("td input:checked").length == 1) {
                selectedRows.push(this);
                selectedNyms.push($(this).find("td a").data("nym"));
            }
        });

        // Disable buttons
        $("#delete-selected-button").prop("disabled", true);
        $("#export-selected-button").prop("disabled", true);

        Mixmesh.post(
            "/key/delete",
            selectedNyms,
            function(data, textStatus, _jqXHR) {
                console.log("/key/delete (POST) succeeded");
                console.log(selectedNyms + " has been deleted");

                // Remove deleted keys
                for (i = 0; i < selectedRows.length; i++) {
                    $(selectedRows[i]).remove();
                }
            },
            function(jqXHR, textStatus, errorThrown) {
                console.log("/key/delete (POST) failed");
                console.log("textStatus: " + textStatus);
                console.log("errorThrown: " + errorThrown);

                Mixmesh.showGenericDialog({
                    title: "Could not delete contacts",
                    content: "<p>" + Mixmesh.formatError(
                        jqXHR, textStatus, errorThrown) + "</p>",
                    onok: function() {
                        Mixmesh.hideGenericDialog();
                    }
                });

                // Enable buttons again
                $("#delete-selected-button").prop("disabled", false);
                $("#export-selected-button").prop("disabled", false);
            });
    });

    // Add handler to export-selected button
    $("#export-selected-button").click(function() {
        var selectedCheckboxes = [];
        var selectedNyms = [];

        // Collect selected keys
        $("#key-table-body tr").each(function() {
            var checkboxes = $(this).find("td input:checked");
            if (checkboxes.length == 1) {
                selectedCheckboxes.push(checkboxes[0]);
                selectedNyms.push($(this).find("td a").data("nym"));
            }
        });

        // Disable buttons
        $("#delete-selected-button").prop("disabled", true);
        $("#export-selected-button").prop("disabled", true);

        Mixmesh.post(
            "/key/export",
            selectedNyms,
            function(data, textStatus, _jqXHR) {
                console.log("/key/export (POST) succeeded");
                console.log(selectedNyms + " has been exported to " + data);

                // Clear checkboxes
                for (i = 0; i < selectedCheckboxes.length; i++) {
                    $(selectedCheckboxes[i]).prop("checked", false);
                }
                $("#select-all").prop("checked", false);

                Mixmesh.showGenericDialog({
                    title: "Export succeeded",
                    content: "<p>You have exported " + Mixmesh.formatAmount(data.size, "contact") + " to a file named <a href=\"" + data["uri-path"] + "\">" + data["uri-path"] + "</a>. Download it and use it to perform a reinstall of a box and/or give them to a friend.</p>",
                    onok: function() {
                        Mixmesh.hideGenericDialog();
                    }
                });
            },
            function(jqXHR, textStatus, errorThrown) {
                console.log("/key/export (POST) failed");
                console.log("textStatus: " + textStatus);
                console.log("errorThrown: " + errorThrown);

                // Enable buttons again
                $("#delete-selected-button").prop("disabled", false);
                $("#export-selected-button").prop("disabled", false);
                Mixmesh.showGenericDialog({
                    title: "Export failed",
                    content: "<p>" + Mixmesh.formatError(
                        jqXHR, textStatus, errorThrown) + "</p>",
                    onok: function() {
                        Mixmesh.hideGenericDialog();
                    }
                });
            });
    });

    // Add handler to export-all button
    $("#export-all-button").click(function() {
        Mixmesh.post(
            "/key/export",
            "all",
            function(data, textStatus, _jqXHR) {
                console.log("/key/export (POST) succeeded");
                console.log("All contacts have been exported to " + data);

                Mixmesh.showGenericDialog({
                    title: "Export succeeded",
                    content: "<p>You have exported " + Mixmesh.formatAmount(data.size, "contact") + " to a file named <a href=\"" + data["uri-path"] + "\">" + data["uri-path"] + "</a>. Download it and use it to perform a reinstall of a box and/or give them to a friend.</p>",
                    onok: function() {
                        Mixmesh.hideGenericDialog();
                    }
                });
            },
            function(jqXHR, textStatus, errorThrown) {
                console.log("/key/export (POST) failed");
                console.log("textStatus: " + textStatus);
                console.log("errorThrown: " + errorThrown);
                Mixmesh.showGenericDialog({
                    title: "Export failed",
                    content: "<p>" + Mixmesh.formatError(
                        jqXHR, textStatus, errorThrown) + "</p>",
                    onok: function() {
                        Mixmesh.hideGenericDialog();
                    }
                });
            });
    });

    Contacts.refreshRows();
});

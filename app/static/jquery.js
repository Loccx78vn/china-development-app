// Handle Tab Switching
$('#overviewTab').on('click', function() {
    switchTab("overview");
});
$('#mapTab').on('click', function() {
    switchTab("map");
});
$('#stockTab').on('click', function() {
    switchTab("stock");
});

// Handle Sub-tab Switching (Static / Interactive)
$('#staticTab').on('click', function() {
    switchSubTab("Static");
});
$('#interactiveTab').on('click', function() {
    switchSubTab("Interactive");
});

function switchTab(tab) {
    // Hide all content sections
    $('.tab-content').hide();
    $('.sub-tab-content').hide();

    // Hide sub-tabs by default when switching to a tab other than Map
    $('#subTabsContainer').hide();

    // Reset active tab styling
    $('.tab').removeClass('active');

    // Hide placeholders by default
    $('#yearFilterPlaceholder').hide();
    $('#companyFilterPlaceholder').hide();

    // Show the appropriate content for the selected tab
    switch (tab) {
        case "overview":
            $('#Overview').show();
            $(".sidebar-left").show();
            $(".content-area").show();
            $('#overviewTab').addClass('active');
            break;

        case "map":
            $('#Map').show();
            $('#subTabsContainer').show(); // Show sub-tabs for Map
            $('#mapTab').addClass('active');
            switchSubTab("Static"); // Default to Static sub-tab
            $('#yearFilterPlaceholder').show(); // Show year filter
            break;

        case "stock":
            $('#Stock').show();
            $(".sidebar-left").show();
            $(".content-area").show();
            $('#stockTab').addClass('active');
            $('#companyFilterPlaceholder').show(); // Show company filter
            break;
    }
}


function switchSubTab(subTab) {
    // Hide all sub-tab content
    $('.sub-tab-content').hide();

    // Show the selected sub-tab content
    $('#' + subTab).show();

    // Remove 'active' class from all sub-tabs
    $('.sub-tab').removeClass('active');

    // Add 'active' class to the clicked tab
    $('#' + subTab.toLowerCase() + "Tab").addClass('active');
}

// Set default active tab (Overview) on page load
$(document).ready(function() {
    switchTab("overview");
});

function toggleTable() {
    $('#tablePlaceholder').css({
        visibility: 'visible',
        display: 'flex'
    }); // Show the placeholder (centered)
}

function closeTable() {
    $('#tablePlaceholder').css({
        visibility: 'hidden',
        display: 'none'
    }); // Hide the placeholder
}

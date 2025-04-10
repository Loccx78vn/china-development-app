// Handle Tab Switching
document.getElementById('overviewTab').addEventListener('click', function() {
    switchTab("overview");
});
document.getElementById('mapTab').addEventListener('click', function() {
    switchTab("map");
});
document.getElementById('stockTab').addEventListener('click', function() {
    switchTab("stock");
});

// Handle Sub-tab Switching (Static / Interactive)
document.getElementById('staticTab').addEventListener('click', function() {
    switchSubTab("Static");
});
document.getElementById('interactiveTab').addEventListener('click', function() {
    switchSubTab("Interactive");
});

function switchTab(tab) {
    // Hide all content sections
    var tabContents = document.querySelectorAll('.tab-content');
    tabContents.forEach(function(content) {
        content.style.display = 'none';
    });

    var subTabContents = document.querySelectorAll('.sub-tab-content');
    subTabContents.forEach(function(content) {
        content.style.display = 'none';
    });

    // Hide sub-tabs by default when switching to a tab other than Map
    document.getElementById('subTabsContainer').style.display = 'none';

    // Reset active tab styling
    var tabs = document.querySelectorAll('.tab');
    tabs.forEach(function(tab) {
        tab.classList.remove('active');
    });

    // Hide placeholders by default
    document.getElementById('yearFilterPlaceholder').style.display = 'none';
    document.getElementById('companyFilterPlaceholder').style.display = 'none';

    // Show the appropriate content for the selected tab
    switch (tab) {
        case "overview":
            document.getElementById('Overview').style.display = 'block';
            document.querySelector(".sidebar-left").style.display = 'block';
            document.querySelector(".content-area").style.display = 'block';
            document.getElementById('overviewTab').classList.add('active');
            break;

        case "map":
            document.getElementById('Map').style.display = 'block';
            document.getElementById('subTabsContainer').style.display = 'block'; // Show sub-tabs for Map
            document.getElementById('mapTab').classList.add('active');
            switchSubTab("Static"); // Default to Static sub-tab
            document.getElementById('yearFilterPlaceholder').style.display = 'block'; // Show year filter
            break;

        case "stock":
            document.getElementById('Stock').style.display = 'block';
            document.querySelector(".sidebar-left").style.display = 'block';
            document.querySelector(".content-area").style.display = 'block';
            document.getElementById('stockTab').classList.add('active');
            document.getElementById('companyFilterPlaceholder').style.display = 'block'; // Show company filter
            break;
    }
}

function switchSubTab(subTab) {
    // Hide all sub-tab content
    var subTabContents = document.querySelectorAll('.sub-tab-content');
    subTabContents.forEach(function(content) {
        content.style.display = 'none';
    });

    // Show the selected sub-tab content
    document.getElementById(subTab).style.display = 'block';

    // Remove 'active' class from all sub-tabs
    var subTabs = document.querySelectorAll('.sub-tab');
    subTabs.forEach(function(tab) {
        tab.classList.remove('active');
    });

    // Add 'active' class to the clicked tab
    document.getElementById(subTab.toLowerCase() + "Tab").classList.add('active');
}

// Set default active tab (Overview) on page load
document.addEventListener('DOMContentLoaded', function() {
    switchTab("overview");
});

function toggleTable() {
    document.getElementById('tablePlaceholder').style.visibility = 'visible';
    document.getElementById('tablePlaceholder').style.display = 'flex'; // Show the placeholder (centered)
}

function closeTable() {
    document.getElementById('tablePlaceholder').style.visibility = 'hidden';
    document.getElementById('tablePlaceholder').style.display = 'none'; // Hide the placeholder
}

document.addEventListener('DOMContentLoaded', function() {
    // Wait until dynamic content is rendered by the shiny app before proceeding
    const observer = new MutationObserver(function(mutationsList, observer) {
        // Check if the critical elements are available
        const overviewTab = document.getElementById('overviewTab');
        const mapTab = document.getElementById('mapTab');
        const stockTab = document.getElementById('stockTab');
        const staticTab = document.getElementById('staticTab');
        const interactiveTab = document.getElementById('interactiveTab');

        // If these elements are now available, stop the observer and proceed
        if (overviewTab && mapTab && stockTab && staticTab && interactiveTab) {
            // Stop observing for changes once the necessary elements are loaded
            observer.disconnect();

            // Add event listeners for the tabs and sub-tabs
            overviewTab.addEventListener('click', function() {
                switchTab("overview");
            });

            mapTab.addEventListener('click', function() {
                switchTab("map");
            });

            stockTab.addEventListener('click', function() {
                switchTab("stock");
            });

            staticTab.addEventListener('click', function() {
                switchSubTab("Static");
            });

            interactiveTab.addEventListener('click', function() {
                switchSubTab("Interactive");
            });

            // Initialize default tab (Overview)
            switchTab("overview");
        }
    });

    // Start observing the document for added child elements (dynamically loaded content)
    observer.observe(document.body, {
        childList: true,  // Observe child nodes being added or removed
        subtree: true     // Also observe subtrees of existing nodes
    });
});

function switchTab(tab) {
    // Hide all content sections
    const tabContents = document.querySelectorAll('.tab-content');
    tabContents.forEach(function(content) {
        content.style.display = 'none';
    });

    const subTabContents = document.querySelectorAll('.sub-tab-content');
    subTabContents.forEach(function(content) {
        content.style.display = 'none';
    });

    // Hide sub-tabs by default when switching to a tab other than Map
    const subTabsContainer = document.querySelector('#subTabsContainer');
    subTabsContainer.style.display = 'none';

    // Reset active tab styling
    const tabs = document.querySelectorAll('.tab');
    tabs.forEach(function(tab) {
        tab.classList.remove('active');
    });

    // Hide placeholders by default
    const yearFilterPlaceholder = document.querySelector('#yearFilterPlaceholder');
    const companyFilterPlaceholder = document.querySelector('#companyFilterPlaceholder');
    yearFilterPlaceholder.style.display = 'none';
    companyFilterPlaceholder.style.display = 'none';

    // Show the appropriate content for the selected tab
    switch (tab) {
        case "overview":
            document.querySelector('#Overview').style.display = 'block';
            document.querySelector(".sidebar-left").style.display = 'block';
            document.querySelector(".content-area").style.display = 'block';
            document.querySelector('#overviewTab').classList.add('active');
            break;

        case "map":
            document.querySelector('#Map').style.display = 'block';
            subTabsContainer.style.display = 'block'; // Show sub-tabs for Map
            document.querySelector('#mapTab').classList.add('active');
            switchSubTab("Static"); // Default to Static sub-tab
            yearFilterPlaceholder.style.display = 'block'; // Show year filter
            break;

        case "stock":
            document.querySelector('#Stock').style.display = 'block';
            document.querySelector(".sidebar-left").style.display = 'block';
            document.querySelector(".content-area").style.display = 'block';
            document.querySelector('#stockTab').classList.add('active');
            companyFilterPlaceholder.style.display = 'block'; // Show company filter
            break;
    }
}

function switchSubTab(subTab) {
    // Hide all sub-tab content
    const subTabContents = document.querySelectorAll('.sub-tab-content');
    subTabContents.forEach(function(content) {
        content.style.display = 'none';
    });

    // Show the selected sub-tab content
    document.querySelector(#${subTab}).style.display = 'block';

    // Remove 'active' class from all sub-tabs
    const subTabs = document.querySelectorAll('.sub-tab');
    subTabs.forEach(function(tab) {
        tab.classList.remove('active');
    });

    // Add 'active' class to the clicked tab
    document.querySelector(#${subTab.toLowerCase()}Tab).classList.add('active');
}

function toggleTable() {
    document.querySelector('#tablePlaceholder').style.visibility = 'visible';
    document.querySelector('#tablePlaceholder').style.display = 'flex'; // Show the placeholder (centered)
}

function closeTable() {
    document.querySelector('#tablePlaceholder').style.visibility = 'hidden';
    document.querySelector('#tablePlaceholder').style.display = 'none'; // Hide the placeholder
}
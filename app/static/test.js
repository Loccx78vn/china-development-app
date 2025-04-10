document.addEventListener("DOMContentLoaded", function () {
    const loadingMessage = document.getElementById("loadingMessage");
    const progressBar = document.getElementById("progressBar");
    const startTime = performance.now();
    preloadAllContent();

    function preloadAllContent() {
        const tabContents = document.querySelectorAll(".tab-content");
        const subTabContents = document.querySelectorAll(".sub-tab-content");
        tabContents.forEach((tabContent) => {
            tabContent.style.display = "block";
        });
        subTabContents.forEach((subTabContent) => {
            subTabContent.style.display = "block";
        });

        const updateProgress = setInterval(function () {
            const elapsedTime = performance.now() - startTime;
            const progressPercentage = Math.min((elapsedTime / 30000) * 100, 100);
            progressBar.style.width = `${progressPercentage}%`;
            if (elapsedTime < 5000) {
                loadingMessage.textContent = "Starting to render all content...";
            } else if (elapsedTime < 10000) {
                loadingMessage.textContent = "Preparing data for charts...";
            } else if (elapsedTime < 15000) {
                loadingMessage.textContent = "Fetching stock and map data...";
            } else if (elapsedTime < 20000) {
                loadingMessage.textContent = "Processing data and optimizing visuals...";
            } else if (elapsedTime < 25000) {
                loadingMessage.textContent = "Finalizing your view...";
            } else {
                loadingMessage.textContent = "Almost ready, just a few more seconds...";
            }
            if (elapsedTime >= 30000) {
                clearInterval(updateProgress);
            }
        }, 500);

        // Initialize the overview and static tabs
        document.getElementById("overviewTab").classList.add("active");
        document.getElementById("Overview").style.display = "block";
        document.getElementById("staticTab").classList.add("active");
        document.getElementById("Static").style.display = "block";
        document.getElementById("subTabsContainer").style.display = "none";
        document.getElementById("yearFilterPlaceholder").style.display = "none";
        document.getElementById("companyFilterPlaceholder").style.display = "none";

        setTimeout(function () {
            document.getElementById("loadingScreen").style.display = "none";
            document.getElementById("contentWrapper").style.visibility = "visible";
            const overviewTabButton = document.getElementById("overviewTab");
            switchTab({ target: overviewTabButton }, "Overview");
            const endTime = performance.now();
            const renderingTime = endTime - startTime;
            console.log(`Content rendered in: ${renderingTime.toFixed(2)} milliseconds`);
        }, 30000);
    }

    function switchTab(event, tabName) {
        const allTabs = document.querySelectorAll(".tab");
        const allTabContents = document.querySelectorAll(".tab-content");
        const subTabContainers = document.querySelectorAll(".sub-tab-content");

        // Remove active class from all tabs and hide their content
        allTabs.forEach((tab) => tab.classList.remove("active"));
        allTabContents.forEach((tabContent) => (tabContent.style.display = "none"));
        subTabContainers.forEach((subTab) => (subTab.style.display = "none"));

        // Add active class to clicked tab and show its content
        event.target.classList.add("active");
        const currentTabContent = document.getElementById(tabName);
        currentTabContent.style.display = "block";

        // Handle specific case for Map Tab
        if (tabName === "Map") {
            document.getElementById("subTabsContainer").style.display = "block";  // Show sub-tabs container
            switchSubTab("Static");  // Ensure Static sub-tab is active by default
            document.getElementById("yearFilterPlaceholder").style.display = "block";
            document.getElementById("companyFilterPlaceholder").style.display = "none";
        } else {
            document.getElementById("subTabsContainer").style.display = "none";  // Hide sub-tabs container for other tabs
            document.getElementById("yearFilterPlaceholder").style.display = "none";
            if (tabName === "Stock") {
                document.getElementById("companyFilterPlaceholder").style.display = "block";
            } else {
                document.getElementById("companyFilterPlaceholder").style.display = "none";
            }
        }
    }

    function switchSubTab(subTab) {
        // Hide all sub-tab content
        document.querySelectorAll('.sub-tab-content').forEach(function(content) {
            content.style.display = 'none';
        });

        // Show the selected sub-tab content
        document.getElementById(subTab).style.display = 'block';

        // Remove 'active' class from all sub-tabs
        const tabs = document.querySelectorAll('.sub-tab');
        tabs.forEach(function(tab) {
            tab.classList.remove('active');
        });

        // Add 'active' class to the clicked sub-tab button
        document.getElementById(subTab.toLowerCase() + "Tab").classList.add('active');
    }

    // Event listeners for switching tabs
    document.getElementById("overviewTab").addEventListener("click", (event) => switchTab(event, "Overview"));
    document.getElementById("mapTab").addEventListener("click", (event) => switchTab(event, "Map"));
    document.getElementById("stockTab").addEventListener("click", (event) => switchTab(event, "Stock"));

    // Event listeners for switching sub-tabs (inside Map tab)
    document.getElementById("staticTab").addEventListener("click", function() {
        switchSubTab("Static");
    });
    document.getElementById("interactiveTab").addEventListener("click", function() {
        switchSubTab("Interactive");
    });

    // Additional functions for table handling
    function toggleTable() {
        var tablePlaceholder = document.getElementById("tablePlaceholder");
        tablePlaceholder.style.visibility = "visible";
        tablePlaceholder.style.display = "flex";
    }

    function closeTable() {
        var tablePlaceholder = document.getElementById("tablePlaceholder");
        tablePlaceholder.style.visibility = "hidden";
        tablePlaceholder.style.display = "none";
    }

    document.getElementById("showTableBtn").addEventListener("click", toggleTable);
    document.getElementById("overlay").addEventListener("click", closeTable);
});

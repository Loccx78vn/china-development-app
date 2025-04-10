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
        allTabs.forEach((tab) => tab.classList.remove("active"));
        allTabContents.forEach((tabContent) => (tabContent.style.display = "none"));
        subTabContainers.forEach((subTab) => (subTab.style.display = "none"));
        event.target.classList.add("active");
        const currentTabContent = document.getElementById(tabName);
        currentTabContent.style.display = "block";
        if (tabName === "Map") {
            document.getElementById("subTabsContainer").style.display = "block";
            switchSubTab("Static");
            document.getElementById("yearFilterPlaceholder").style.display = "block";
            document.getElementById("companyFilterPlaceholder").style.display = "none";
            document.getElementById("staticTab").classList.add("active");
        } else {
            document.getElementById("subTabsContainer").style.display = "none";
            document.getElementById("yearFilterPlaceholder").style.display = "none";
            if (tabName === "Stock") {
                document.getElementById("companyFilterPlaceholder").style.display = "block";
            } else {
                document.getElementById("companyFilterPlaceholder").style.display = "none";
            }
        }
    }
    function switchSubTab(tabName) {
        const allSubTabs = document.querySelectorAll(".sub-tab");
        const allSubTabContents = document.querySelectorAll(".sub-tab-content");
        allSubTabs.forEach((subTab) => subTab.classList.remove("active"));
        allSubTabContents.forEach((subTabContent) => (subTabContent.style.display = "none"));
        const activeSubTab = document.getElementById(tabName);
        activeSubTab.classList.add("active");
        const activeSubTabContent = document.getElementById(tabName);
        activeSubTabContent.style.display = "block";
    }
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
    document.getElementById("overviewTab").addEventListener("click", (event) => switchTab(event, "Overview"));
    document.getElementById("mapTab").addEventListener("click", (event) => switchTab(event, "Map"));
    document.getElementById("stockTab").addEventListener("click", (event) => switchTab(event, "Stock"));
    document.getElementById("staticTab").addEventListener("click", () => switchSubTab("Static"));
    document.getElementById("interactiveTab").addEventListener("click", () => switchSubTab("Interactive"));
});

function toggleTable() {
    console.log("Toggling table visibility");
    var tablePlaceholder = document.getElementById("tablePlaceholder");
    tablePlaceholder.style.visibility = "visible";
    tablePlaceholder.style.display = "flex";
}
function closeTable() {
    console.log("Closing table");
    var tablePlaceholder = document.getElementById("tablePlaceholder");
    tablePlaceholder.style.visibility = "hidden";
    tablePlaceholder.style.display = "none";
}
document.addEventListener("DOMContentLoaded", function () {
    document.getElementById("overviewTab").addEventListener("click", function () {
        console.log("Overview tab clicked");
        switchTab("overview");
    });
    document.getElementById("mapTab").addEventListener("click", function () {
        console.log("Map tab clicked");
        switchTab("map");
    });
    document.getElementById("stockTab").addEventListener("click", function () {
        console.log("Stock tab clicked");
        switchTab("stock");
    });
    document.getElementById("staticTab").addEventListener("click", function () {
        console.log("Static sub-tab clicked");
        switchSubTab("Static");
    });
    document.getElementById("interactiveTab").addEventListener("click", function () {
        console.log("Interactive sub-tab clicked");
        switchSubTab("Interactive");
    });
    switchTab("overview");
    function switchTab(tab) {
        console.log("Switching to tab:", tab);
        document.querySelectorAll(".tab-content").forEach(function (content) {
            content.style.display = "none";
        });
        document.querySelectorAll(".sub-tab-content").forEach(function (content) {
            content.style.display = "none";
        });
        document.getElementById("subTabsContainer").style.display = "none";
        const tabs = document.querySelectorAll(".tab");
        tabs.forEach(function (tab) {
            tab.classList.remove("active");
        });
        const yearFilterPlaceholder = document.getElementById("yearFilterPlaceholder");
        const companyFilterPlaceholder = document.getElementById("companyFilterPlaceholder");
        yearFilterPlaceholder.style.display = "none";
        companyFilterPlaceholder.style.display = "none";
        if (tab === "overview") {
            console.log("Displaying Overview Tab");
            document.getElementById("Overview").style.display = "block";
            document.querySelector(".sidebar-left").style.display = "block";
            document.querySelector(".content-area").style.display = "block";
            document.getElementById("overviewTab").classList.add("active");
        } else if (tab === "map") {
            console.log("Displaying Map Tab");
            document.getElementById("Map").style.display = "block";
            document.getElementById("subTabsContainer").style.display = "flex";
            document.getElementById("mapTab").classList.add("active");
            switchSubTab("Static");
            yearFilterPlaceholder.style.display = "block";
        } else if (tab === "stock") {
            console.log("Displaying Stock Tab");
            document.getElementById("Stock").style.display = "block";
            document.querySelector(".sidebar-left").style.display = "block";
            document.querySelector(".content-area").style.display = "block";
            document.getElementById("stockTab").classList.add("active");
            companyFilterPlaceholder.style.display = "block";
        }
    }
    function switchSubTab(subTab) {
        console.log("Switching to sub-tab:", subTab);
        document.querySelectorAll(".sub-tab-content").forEach(function (content) {
            content.style.display = "none";
        });
        document.getElementById(subTab).style.display = "block";
        const tabs = document.querySelectorAll(".sub-tab");
        tabs.forEach(function (tab) {
            tab.classList.remove("active");
        });
        document.getElementById(subTab.toLowerCase() + "Tab").classList.add("active");
    }
});

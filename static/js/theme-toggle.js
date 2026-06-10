// Manual light/dark theme toggle.
// The inline <head> script sets the initial theme from the saved choice
// (localStorage 'theme') or the OS preference. This file only handles the
// button click and the mobile menu. No geolocation, no auto day/night.

function updateMode() {
    // Sync the .dark class to whatever is stored / preferred. Called on load.
    var saved = localStorage.getItem('theme');
    var dark = saved
        ? saved === 'dark'
        : window.matchMedia('(prefers-color-scheme: dark)').matches;
    document.documentElement.classList.toggle('dark', dark);
}

function toggleMode() {
    var dark = !document.documentElement.classList.contains('dark');
    document.documentElement.classList.toggle('dark', dark);
    localStorage.setItem('theme', dark ? 'dark' : 'light');
}

function toggleMenu() {
    var navbar = document.getElementById('navbar-default');
    if (navbar) {
        navbar.classList.toggle('hidden');
    }
}

if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', updateMode);
} else {
    updateMode();
}

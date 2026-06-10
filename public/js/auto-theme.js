// Automatic day/night theme based on user's sunrise/sunset
class AutoTheme {
    constructor() {
        this.latitude = null;
        this.longitude = null;
        this.manualOverride = localStorage.getItem('theme-manual-override');
        this.init();
    }

    init() {
        // If user has manually set a theme, respect it (already applied by head script)
        if (this.manualOverride) {
            return;
        }

        // Try to get user's location for sunrise/sunset calculation
        // Don't touch the theme until geolocation resolves — the head
        // script already set a correct initial theme from system preference.
        this.getUserLocation();
    }

    getUserLocation() {
        if (navigator.geolocation) {
            navigator.geolocation.getCurrentPosition(
                (position) => {
                    this.latitude = position.coords.latitude;
                    this.longitude = position.coords.longitude;
                    this.updateThemeBasedOnTime();
                    // Update every minute
                    setInterval(() => this.updateThemeBasedOnTime(), 60000);
                },
                () => {
                    // Fallback to system preference if location denied
                    this.fallbackToSystemPreference();
                }
            );
        } else {
            // Fallback to system preference if geolocation not supported
            this.fallbackToSystemPreference();
        }
    }

    fallbackToSystemPreference() {
        const prefersDark = window.matchMedia('(prefers-color-scheme: dark)').matches;
        this.updateMode(prefersDark ? 'dark' : 'light');
        
        // Listen for system preference changes
        window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', (e) => {
            if (!this.manualOverride) {
                this.updateMode(e.matches ? 'dark' : 'light');
            }
        });
    }

    calculateSunTimes(lat, lng, date = new Date()) {
        // Simplified sunrise/sunset calculation
        const J = this.getJulianDay(date);
        const n = J - 2451545.0 + 0.0008;
        const L = (280.460 + 0.9856474 * n) % 360;
        const g = ((357.528 + 0.9856003 * n) % 360) * Math.PI / 180;
        const lambda = (L + 1.915 * Math.sin(g) + 0.020 * Math.sin(2 * g)) * Math.PI / 180;
        
        const decl = Math.asin(Math.sin(lambda) * Math.sin(23.45 * Math.PI / 180));
        const latRad = lat * Math.PI / 180;
        
        const hourAngle = Math.acos(-Math.tan(latRad) * Math.tan(decl));
        const sunrise = 12 - hourAngle * 12 / Math.PI;
        const sunset = 12 + hourAngle * 12 / Math.PI;
        
        return {
            sunrise: this.hoursToTime(sunrise, lng),
            sunset: this.hoursToTime(sunset, lng)
        };
    }

    getJulianDay(date) {
        const a = Math.floor((14 - (date.getMonth() + 1)) / 12);
        const y = date.getFullYear() + 4800 - a;
        const m = (date.getMonth() + 1) + 12 * a - 3;
        return date.getDate() + Math.floor((153 * m + 2) / 5) + 365 * y + Math.floor(y / 4) - Math.floor(y / 100) + Math.floor(y / 400) - 32045;
    }

    hoursToTime(hours, lng) {
        // Adjust for timezone (rough approximation)
        const timezoneOffset = lng / 15;
        const adjustedHours = hours + timezoneOffset;
        const date = new Date();
        date.setHours(Math.floor(adjustedHours));
        date.setMinutes((adjustedHours % 1) * 60);
        date.setSeconds(0);
        return date;
    }

    updateThemeBasedOnTime() {
        if (!this.latitude || !this.longitude) return;

        const now = new Date();
        const sunTimes = this.calculateSunTimes(this.latitude, this.longitude, now);
        
        const isNightTime = now < sunTimes.sunrise || now > sunTimes.sunset;
        const theme = isNightTime ? 'dark' : 'light';
        
        this.updateMode(theme);
    }

    updateMode(theme) {
        if (theme === 'dark') {
            document.documentElement.classList.add('dark');
        } else {
            document.documentElement.classList.remove('dark');
        }
        
        // Update the theme toggle button state if it exists
        const themeToggle = document.querySelector('[onclick="toggleMode()"]');
        if (themeToggle) {
            const isDark = theme === 'dark';
            const icon = themeToggle.querySelector('svg use');
            if (icon) {
                icon.setAttribute('href', isDark ? '#light_mode' : '#dark_mode');
            }
        }
    }

    // Manual toggle function (called when user clicks the button)
    toggleMode() {
        const currentTheme = document.documentElement.classList.contains('dark') ? 'dark' : 'light';
        const newTheme = currentTheme === 'dark' ? 'light' : 'dark';
        
        // Set manual override
        localStorage.setItem('theme-manual-override', newTheme);
        this.manualOverride = newTheme;
        
        this.updateMode(newTheme);
        
        // Clear manual override after 24 hours
        setTimeout(() => {
            localStorage.removeItem('theme-manual-override');
            this.manualOverride = null;
            this.updateThemeBasedOnTime();
        }, 24 * 60 * 60 * 1000);
    }
}

// Global functions for compatibility with theme templates
let autoTheme;

function updateMode() {
    // Theme is already set by inline <head> script.
    // Just ensure AutoTheme is initialized for geolocation-based switching.
    if (!autoTheme) {
        autoTheme = new AutoTheme();
    }
}

function toggleMode() {
    if (!autoTheme) {
        autoTheme = new AutoTheme();
    }
    autoTheme.toggleMode();
}

function toggleMenu() {
    let navbar = document.getElementById("navbar-default");
    if (navbar.classList.contains("hidden")) {
        navbar.classList.remove("hidden");
    } else {
        navbar.classList.add("hidden");
    }
}

// Initialize once DOM is ready (not 'load' — that waits for images)
if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', () => {
        autoTheme = new AutoTheme();
    });
} else {
    autoTheme = new AutoTheme();
}
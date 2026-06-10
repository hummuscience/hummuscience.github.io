// Search functionality for notes pages
class NotesSearch {
    constructor() {
        this.searchIndex = [];
        this.isNotesPage = window.location.pathname.includes('/notes');
        
        if (this.isNotesPage) {
            this.init();
        }
    }

    async init() {
        await this.buildSearchIndex();
        this.createSearchInterface();
        this.setupEventListeners();
    }

    async buildSearchIndex() {
        // Create search index from your notes
        this.searchIndex = [
            {
                title: "Psychedelics",
                url: "/notes/20220405102340-psychedelics/",
                content: "psychedelics research animal studies DOI compounds scheduled substances clinical settings recreational use pharmacology serotonin receptors",
                excerpt: "Notes on psychedelic compounds and their research applications"
            },
            {
                title: "DOI",
                url: "/notes/20220405103750-doi/",
                content: "DOI psychedelic compound research animal studies alternatives scheduled substances pharmacology receptor binding",
                excerpt: "Information about DOI compounds used in psychedelic research"
            },
            {
                title: "Lysergic Acid Diethylamide (LSD)",
                url: "/notes/20210518184927-lysergic_acid_diethylamide_lsd/",
                content: "LSD lysergic acid diethylamide psychedelic serotonin 5-HT2A receptor mechanism action research clinical studies",
                excerpt: "Comprehensive notes on LSD mechanisms and research"
            },
            {
                title: "MDMA",
                url: "/notes/20230323085540-mdma/",
                content: "MDMA methylenedioxymethamphetamine psychedelic therapeutic research PTSD clinical trials serotonin dopamine",
                excerpt: "Notes on MDMA therapeutic applications and research"
            },
            {
                title: "5-HT2A Receptor",
                url: "/notes/20220405104217-5_ht2a_receptor/",
                content: "5-HT2A receptor serotonin psychedelic mechanism action binding pharmacology neurotransmitter brain",
                excerpt: "Information about the 5-HT2A serotonin receptor"
            },
            {
                title: "5-HT2C Receptor", 
                url: "/notes/20220405104300-5_ht2c_receptor/",
                content: "5-HT2C receptor serotonin neurotransmitter brain pharmacology binding mechanism psychedelic action",
                excerpt: "Information about the 5-HT2C serotonin receptor"
            }
        ];

        // Try to get more content from the current page and other visible notes
        this.enhanceSearchIndex();
    }

    enhanceSearchIndex() {
        // Get additional content from any notes links visible on the current page
        const noteLinks = document.querySelectorAll('a[href*="/notes/"]');
        const seenUrls = new Set(this.searchIndex.map(item => item.url));

        noteLinks.forEach(link => {
            const url = link.getAttribute('href');
            const title = link.textContent.trim();
            
            if (!seenUrls.has(url) && url && title) {
                this.searchIndex.push({
                    title: title,
                    url: url,
                    content: title.toLowerCase(),
                    excerpt: `Note: ${title}`
                });
                seenUrls.add(url);
            }
        });
    }

    createSearchInterface() {
        const searchContainer = document.createElement('div');
        searchContainer.className = 'notes-search-container';
        searchContainer.innerHTML = `
            <div class="search-box">
                <input type="text" 
                       id="notes-search-input" 
                       placeholder="Search notes..." 
                       autocomplete="off">
                <div class="search-icon">
                    <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                        <circle cx="11" cy="11" r="8"></circle>
                        <Path d="m21 21-4.35-4.35"></Path>
                    </svg>
                </div>
            </div>
            <div id="search-results" class="search-results hidden"></div>
        `;

        // Add CSS for search interface
        const style = document.createElement('style');
        style.textContent = `
            .notes-search-container {
                position: fixed;
                top: 90px;
                right: 20px;
                width: 350px;
                z-index: 1000;
                font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
            }

            .search-box {
                position: relative;
                margin-bottom: 10px;
            }

            #notes-search-input {
                width: 100%;
                padding: 12px 16px 12px 40px;
                border: 2px solid var(--border, #ece7de);
                border-radius: 8px;
                font-size: 14px;
                background: white;
                box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
                transition: all 0.2s ease;
                box-sizing: border-box;
            }

            #notes-search-input:focus {
                outline: none;
                border-color: var(--accent, #b08d57);
                box-shadow: 0 2px 12px rgba(176, 141, 87, 0.25);
            }

            .search-icon {
                position: absolute;
                left: 12px;
                top: 50%;
                transform: translateY(-50%);
                color: var(--text-muted, #8a8378);
                pointer-events: none;
            }

            .search-results {
                background: white;
                border: 1px solid var(--border, #ece7de);
                border-radius: 8px;
                box-shadow: 0 4px 20px rgba(0, 0, 0, 0.15);
                max-height: 400px;
                overflow-y: auto;
            }

            .search-results.hidden {
                display: none;
            }

            .search-result-item {
                padding: 12px 16px;
                border-bottom: 1px solid var(--border, #ece7de);
                cursor: pointer;
                transition: background-color 0.2s ease;
            }

            .search-result-item:last-child {
                border-bottom: none;
            }

            .search-result-item:hover {
                background-color: var(--bg, #fdfbf7);
            }

            .search-result-item.highlighted {
                background-color: var(--thumb-bg, #e8e2d8);
            }

            .search-result-title {
                font-weight: 600;
                color: var(--text, #1a1a1a);
                margin-bottom: 4px;
                font-size: 14px;
            }

            .search-result-excerpt {
                color: var(--text-muted, #8a8378);
                font-size: 12px;
                line-height: 1.4;
            }

            .search-result-url {
                color: var(--accent, #b08d57);
                font-size: 11px;
                margin-top: 4px;
            }

            .no-results {
                padding: 16px;
                text-align: center;
                color: var(--text-muted, #8a8378);
                font-style: italic;
                font-size: 14px;
            }

            /* Dark mode support */
            .dark .notes-search-container #notes-search-input {
                background: var(--bg-card, #1f1c15);
                border-color: var(--border, #2c2820);
                color: var(--text, #e8e4dc);
            }

            .dark .notes-search-container #notes-search-input:focus {
                border-color: var(--accent, #c9a36a);
            }

            .dark .search-results {
                background: var(--bg-card, #1f1c15);
                border-color: var(--border, #2c2820);
            }

            .dark .search-result-item {
                border-color: var(--border, #2c2820);
            }

            .dark .search-result-item:hover {
                background-color: var(--border, #2c2820);
            }

            .dark .search-result-item.highlighted {
                background-color: var(--bg-card, #1f1c15);
            }

            .dark .search-result-title {
                color: var(--accent, #c9a36a);
            }

            .dark .search-result-excerpt {
                color: var(--border, #ece7de);
            }

            .dark .search-result-url {
                color: var(--accent, #c9a36a);
            }

            .dark .no-results {
                color: var(--border, #ece7de);
            }

            /* Mobile responsiveness */
            @media (max-width: 768px) {
                .notes-search-container {
                    top: 10px;
                    right: 10px;
                    left: 10px;
                    width: auto;
                }
            }

            /* Hide on very small screens */
            @media (max-width: 480px) {
                .notes-search-container {
                    display: none;
                }
            }
        `;

        document.head.appendChild(style);
        document.body.appendChild(searchContainer);
        
        // Add body class to indicate right sidebar is present
        document.body.classList.add('has-right-sidebar');
    }

    setupEventListeners() {
        const searchInput = document.getElementById('notes-search-input');
        const searchResults = document.getElementById('search-results');
        let currentHighlight = -1;

        searchInput.addEventListener('input', (e) => {
            const query = e.target.value.trim();
            currentHighlight = -1;
            
            if (query.length < 2) {
                searchResults.classList.add('hidden');
                return;
            }

            this.performSearch(query);
        });

        searchInput.addEventListener('keydown', (e) => {
            const resultItems = searchResults.querySelectorAll('.search-result-item');
            
            if (e.key === 'ArrowDown') {
                e.preventDefault();
                currentHighlight = Math.min(currentHighlight + 1, resultItems.length - 1);
                this.updateHighlight(resultItems, currentHighlight);
            } else if (e.key === 'ArrowUp') {
                e.preventDefault();
                currentHighlight = Math.max(currentHighlight - 1, -1);
                this.updateHighlight(resultItems, currentHighlight);
            } else if (e.key === 'Enter') {
                e.preventDefault();
                if (currentHighlight >= 0 && resultItems[currentHighlight]) {
                    const link = resultItems[currentHighlight].querySelector('.search-result-title');
                    if (link) {
                        window.location.href = resultItems[currentHighlight].dataset.url;
                    }
                }
            } else if (e.key === 'Escape') {
                searchResults.classList.add('hidden');
                searchInput.blur();
            }
        });

        // Hide search results when clicking outside
        document.addEventListener('click', (e) => {
            if (!e.target.closest('.notes-search-container')) {
                searchResults.classList.add('hidden');
            }
        });
    }

    performSearch(query) {
        const searchResults = document.getElementById('search-results');
        const queryWords = query.toLowerCase().split(/\s+/);
        
        const results = this.searchIndex
            .map(item => {
                let score = 0;
                const titleLower = item.title.toLowerCase();
                const contentLower = item.content.toLowerCase();
                
                // Title matches get higher score
                queryWords.forEach(word => {
                    if (titleLower.includes(word)) {
                        score += titleLower === word ? 100 : 50; // Exact match vs contains
                    }
                    if (contentLower.includes(word)) {
                        score += 10;
                    }
                });
                
                return { ...item, score };
            })
            .filter(item => item.score > 0)
            .sort((a, b) => b.score - a.score)
            .slice(0, 8); // Limit to 8 results

        this.displayResults(results, query);
    }

    displayResults(results, query) {
        const searchResults = document.getElementById('search-results');
        
        if (results.length === 0) {
            searchResults.innerHTML = '<div class="no-results">No notes found</div>';
        } else {
            searchResults.innerHTML = results.map(result => `
                <div class="search-result-item" data-url="${result.url}">
                    <div class="search-result-title">${this.highlightText(result.title, query)}</div>
                    <div class="search-result-excerpt">${this.highlightText(result.excerpt, query)}</div>
                    <div class="search-result-url">${result.url}</div>
                </div>
            `).join('');

            // Add click handlers
            searchResults.querySelectorAll('.search-result-item').forEach(item => {
                item.addEventListener('click', () => {
                    window.location.href = item.dataset.url;
                });
            });
        }
        
        searchResults.classList.remove('hidden');
    }

    highlightText(text, query) {
        if (!query) return text;
        
        const queryWords = query.split(/\s+/);
        let highlightedText = text;
        
        queryWords.forEach(word => {
            const regex = new RegExp(`(${word})`, 'gi');
            highlightedText = highlightedText.replace(regex, '<mark style="background: var(--thumb-bg, #e8e2d8); padding: 0;">$1</mark>');
        });
        
        return highlightedText;
    }

    updateHighlight(items, index) {
        items.forEach((item, i) => {
            if (i === index) {
                item.classList.add('highlighted');
            } else {
                item.classList.remove('highlighted');
            }
        });
    }
}

// Initialize search when page loads
document.addEventListener('DOMContentLoaded', () => {
    new NotesSearch();
});
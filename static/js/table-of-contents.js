// Table of Contents system for blog posts, notes, and projects
class TableOfContents {
    constructor() {
        this.minHeadings = 2; // Minimum number of headings to show TOC
        this.tocContainer = null;
        this.headings = [];
        this.currentActiveHeading = null;
        
        // Only initialize on content pages (not homepage)
        if (this.isContentPage()) {
            this.init();
        }
    }

    isContentPage() {
        const path = window.location.pathname;
        return path.includes('/blog/') || 
               path.includes('/notes/') || 
               path.includes('/projects/') ||
               (path.includes('/') && path !== '/' && !path.includes('/tags/') && !path.includes('/categories/'));
    }

    init() {
        this.collectHeadings();
        if (this.headings.length >= this.minHeadings) {
            this.createTOC();
            this.setupScrollSpy();
            this.adjustContentMargin();
        }
    }

    collectHeadings() {
        // Find all headings in the main content area
        const contentArea = document.querySelector('main, .content, article') || document.body;
        const headingElements = contentArea.querySelectorAll('h1, h2, h3, h4, h5, h6');
        
        this.headings = Array.from(headingElements).map((heading, index) => {
            // Create ID if it doesn't exist
            if (!heading.id) {
                const text = heading.textContent.trim();
                const id = text.toLowerCase()
                    .replace(/[^\w\s-]/g, '') // Remove special characters
                    .replace(/\s+/g, '-')     // Replace spaces with hyphens
                    .replace(/-+/g, '-')      // Replace multiple hyphens with single
                    .replace(/^-|-$/g, '');   // Remove leading/trailing hyphens
                heading.id = id || `heading-${index}`;
            }

            return {
                element: heading,
                id: heading.id,
                text: heading.textContent.trim(),
                level: parseInt(heading.tagName.charAt(1)),
                offsetTop: heading.offsetTop
            };
        });
    }

    createTOC() {
        // Create TOC container
        this.tocContainer = document.createElement('div');
        this.tocContainer.className = 'table-of-contents';
        this.tocContainer.innerHTML = `
            <div class="toc-header">
                <h3>Contents</h3>
                <button class="toc-toggle" aria-label="Toggle table of contents">
                    <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                        <polyline points="6,9 12,15 18,9"></polyline>
                    </svg>
                </button>
            </div>
            <nav class="toc-nav">
                <ul class="toc-list">
                    ${this.generateTOCItems()}
                </ul>
            </nav>
        `;

        // Add CSS styles
        this.addTOCStyles();

        // Insert TOC into page
        document.body.appendChild(this.tocContainer);
        
        // Add body class to indicate TOC is present
        document.body.classList.add('has-toc');

        // Setup event listeners
        this.setupTOCEventListeners();
    }

    generateTOCItems() {
        if (this.headings.length === 0) return '';

        let tocHTML = '';
        let currentLevel = this.headings[0].level;

        this.headings.forEach((heading, index) => {
            const levelClass = `toc-level-${heading.level}`;
            const isActive = index === 0 ? 'toc-active' : '';
            
            tocHTML += `
                <li class="toc-item ${levelClass}">
                    <a href="#${heading.id}" class="toc-link ${isActive}" data-heading-id="${heading.id}">
                        ${heading.text}
                    </a>
                </li>
            `;
        });

        return tocHTML;
    }

    addTOCStyles() {
        const style = document.createElement('style');
        style.textContent = `
            .table-of-contents {
                position: fixed;
                left: 20px;
                top: 20%;
                width: 280px;
                max-height: 70vh;
                background: white;
                border: 1px solid var(--border, #ece7de);
                border-radius: 8px;
                box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1);
                z-index: 1000;
                font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
                overflow: hidden;
                transition: all 0.3s ease;
            }

            .table-of-contents.collapsed .toc-nav {
                display: none;
            }

            .toc-header {
                display: flex;
                justify-content: space-between;
                align-items: center;
                padding: 12px 16px;
                border-bottom: 1px solid var(--border, #ece7de);
                background: var(--bg, #fdfbf7);
            }

            .toc-header h3 {
                margin: 0;
                font-size: 14px;
                font-weight: 600;
                color: var(--bg-card, #1f1c15);
            }

            .toc-toggle {
                background: none;
                border: none;
                cursor: pointer;
                padding: 4px;
                border-radius: 4px;
                color: var(--text-muted, #8a8378);
                transition: all 0.2s ease;
            }

            .toc-toggle:hover {
                background: var(--border, #ece7de);
                color: var(--bg-card, #1f1c15);
            }

            .toc-nav {
                max-height: calc(70vh - 60px);
                overflow-y: auto;
                padding: 8px 0;
            }

            .toc-list {
                list-style: none;
                margin: 0;
                padding: 0;
            }

            .toc-item {
                margin: 0;
                padding: 0;
            }

            .toc-link {
                display: block;
                padding: 6px 16px;
                color: var(--text-muted, #8a8378);
                text-decoration: none;
                font-size: 13px;
                line-height: 1.4;
                border-left: 3px solid transparent;
                transition: all 0.2s ease;
            }

            .toc-link:hover {
                color: var(--bg-card, #1f1c15);
                background: var(--thumb-bg, #e8e2d8);
                border-left-color: var(--border, #ece7de);
            }

            .toc-link.toc-active {
                color: var(--accent, #b08d57);
                background: var(--thumb-bg, #e8e2d8);
                border-left-color: var(--accent, #b08d57);
                font-weight: 500;
            }

            /* Heading level indentation */
            .toc-level-1 .toc-link { padding-left: 16px; }
            .toc-level-2 .toc-link { padding-left: 24px; }
            .toc-level-3 .toc-link { padding-left: 32px; }
            .toc-level-4 .toc-link { padding-left: 40px; }
            .toc-level-5 .toc-link { padding-left: 48px; }
            .toc-level-6 .toc-link { padding-left: 56px; }

            /* Dark mode support */
            .dark .table-of-contents {
                background: var(--bg-card, #1f1c15);
                border-color: var(--border, #2c2820);
                box-shadow: 0 4px 12px rgba(0, 0, 0, 0.3);
            }

            .dark .toc-header {
                background: var(--border, #2c2820);
                border-color: var(--text-muted, #8a8378);
            }

            .dark .toc-header h3 {
                color: var(--thumb-bg, #e8e2d8);
            }

            .dark .toc-toggle {
                color: var(--border, #ece7de);
            }

            .dark .toc-toggle:hover {
                background: var(--text-muted, #8a8378);
                color: var(--thumb-bg, #e8e2d8);
            }

            .dark .toc-link {
                color: var(--border, #ece7de);
            }

            .dark .toc-link:hover {
                color: var(--thumb-bg, #e8e2d8);
                background: var(--border, #2c2820);
                border-left-color: var(--text-muted, #8a8378);
            }

            .dark .toc-link.toc-active {
                color: var(--accent, #c9a36a);
                background: var(--bg-card, #1f1c15);
                border-left-color: var(--accent, #c9a36a);
            }

            /* Responsive design */
            @media (max-width: 1200px) {
                .table-of-contents {
                    left: 10px;
                    width: 250px;
                }
            }

            @media (max-width: 1024px) {
                .table-of-contents {
                    display: none;
                }
            }

            /* Main content margins are handled by centered-layout.css */

            /* Smooth scrolling for anchor links */
            html {
                scroll-behavior: smooth;
            }

            /* Highlight target heading briefly when navigated to */
            :target {
                animation: highlight-heading 2s ease-out;
            }

            @keyframes highlight-heading {
                0% { background-color: var(--thumb-bg, #e8e2d8); }
                100% { background-color: transparent; }
            }

            .dark :target {
                animation: highlight-heading-dark 2s ease-out;
            }

            @keyframes highlight-heading-dark {
                0% { background-color: var(--accent, #b08d57); }
                100% { background-color: transparent; }
            }
        `;

        document.head.appendChild(style);
    }

    setupTOCEventListeners() {
        // Toggle TOC collapse/expand
        const toggleButton = this.tocContainer.querySelector('.toc-toggle');
        toggleButton.addEventListener('click', () => {
            this.tocContainer.classList.toggle('collapsed');
            const icon = toggleButton.querySelector('svg polyline');
            if (this.tocContainer.classList.contains('collapsed')) {
                icon.setAttribute('points', '9,6 15,12 9,18');
            } else {
                icon.setAttribute('points', '6,9 12,15 18,9');
            }
        });

        // TOC link clicks
        const tocLinks = this.tocContainer.querySelectorAll('.toc-link');
        tocLinks.forEach(link => {
            link.addEventListener('click', (e) => {
                e.preventDefault();
                const targetId = link.getAttribute('data-heading-id');
                const targetElement = document.getElementById(targetId);
                
                if (targetElement) {
                    // Update active state
                    this.updateActiveHeading(link);
                    
                    // Smooth scroll to target
                    targetElement.scrollIntoView({
                        behavior: 'smooth',
                        block: 'start'
                    });

                    // Update URL without triggering page reload
                    history.pushState(null, null, `#${targetId}`);
                }
            });
        });
    }

    setupScrollSpy() {
        let ticking = false;

        const updateActiveHeadingOnScroll = () => {
            if (!ticking) {
                requestAnimationFrame(() => {
                    this.updateActiveHeadingOnScroll();
                    ticking = false;
                });
                ticking = true;
            }
        };

        window.addEventListener('scroll', updateActiveHeadingOnScroll);
        
        // Initial update
        this.updateActiveHeadingOnScroll();
    }

    updateActiveHeadingOnScroll() {
        const scrollTop = window.pageYOffset || document.documentElement.scrollTop;
        const windowHeight = window.innerHeight;
        const offset = 100; // Offset from top for activation

        let activeHeading = null;

        // Find the current active heading
        for (let i = this.headings.length - 1; i >= 0; i--) {
            const heading = this.headings[i];
            const element = heading.element;
            const rect = element.getBoundingClientRect();
            const elementTop = rect.top + scrollTop;

            if (scrollTop + offset >= elementTop) {
                activeHeading = heading;
                break;
            }
        }

        // If no heading is above the offset, use the first one
        if (!activeHeading && this.headings.length > 0) {
            activeHeading = this.headings[0];
        }

        if (activeHeading && activeHeading !== this.currentActiveHeading) {
            const activeLink = this.tocContainer.querySelector(`[data-heading-id="${activeHeading.id}"]`);
            if (activeLink) {
                this.updateActiveHeading(activeLink);
            }
        }
    }

    updateActiveHeading(activeLink) {
        // Remove active class from all links
        const allLinks = this.tocContainer.querySelectorAll('.toc-link');
        allLinks.forEach(link => link.classList.remove('toc-active'));

        // Add active class to current link
        activeLink.classList.add('toc-active');

        // Scroll the TOC to keep active item visible
        const tocNav = this.tocContainer.querySelector('.toc-nav');
        const linkRect = activeLink.getBoundingClientRect();
        const tocRect = tocNav.getBoundingClientRect();

        if (linkRect.bottom > tocRect.bottom || linkRect.top < tocRect.top) {
            activeLink.scrollIntoView({
                behavior: 'smooth',
                block: 'nearest'
            });
        }

        // Update current active heading
        const headingId = activeLink.getAttribute('data-heading-id');
        this.currentActiveHeading = this.headings.find(h => h.id === headingId);
    }

    adjustContentMargin() {
        // Content margin adjustment is now handled by centered-layout.css
        // This method is kept for compatibility but does nothing
    }
}

// Initialize table of contents when page loads
document.addEventListener('DOMContentLoaded', () => {
    new TableOfContents();
});
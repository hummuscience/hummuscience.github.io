// Backlinks system for org-roam exported content
class BacklinksSystem {
    constructor() {
        this.pages = new Map();
        this.backlinks = new Map();
        this.currentPage = window.location.pathname;
        this.init();
    }

    async init() {
        await this.loadSiteMap();
        await this.parseAllPages();
        this.displayBacklinks();
    }

    async loadSiteMap() {
        try {
            const response = await fetch('/sitemap.xml');
            const text = await response.text();
            const parser = new DOMParser();
            const xml = parser.parseFromString(text, 'text/xml');
            
            const urls = xml.querySelectorAll('url loc');
            urls.forEach(url => {
                const path = new URL(url.textContent).pathname;
                if (path !== '/' && !path.includes('/tags/') && !path.includes('/categories/')) {
                    this.pages.set(path, { url: url.textContent, title: '', content: '' });
                }
            });
        } catch (error) {
            console.log('Could not load sitemap, using current page only');
        }
    }

    async parseAllPages() {
        // Parse current page to find outgoing links
        this.parseCurrentPageLinks();
        
        // For now, we'll create a mapping based on your existing content structure
        // In a full implementation, you'd fetch all pages and parse their links
        this.createLinkDatabase();
        this.findBacklinksForCurrentPage();
    }

    parseCurrentPageLinks() {
        // Find all internal links on the current page
        const outgoingLinks = [];
        const linkElements = document.querySelectorAll('main a[href^="/"]');
        
        linkElements.forEach(link => {
            const href = link.getAttribute('href');
            const text = link.textContent.trim();
            if (href && text && !href.includes('/tags/') && !href.includes('/categories/')) {
                outgoingLinks.push({ 
                    href: this.normalizePath(href), 
                    text, 
                    context: this.getContextAroundLink(link)
                });
            }
        });

        // console.log('Found outgoing links:', outgoingLinks);
        return outgoingLinks;
    }

    normalizePath(path) {
        // Remove trailing slashes and fragments
        return path.replace(/\/$/, '').split('#')[0];
    }

    getContextAroundLink(linkElement) {
        // Get surrounding text for context
        const parent = linkElement.parentElement;
        const text = parent.textContent;
        const linkText = linkElement.textContent;
        const linkIndex = text.indexOf(linkText);
        
        const start = Math.max(0, linkIndex - 50);
        const end = Math.min(text.length, linkIndex + linkText.length + 50);
        
        return '...' + text.substring(start, end).trim() + '...';
    }

    createLinkDatabase() {
        // Create a database of known links between your pages
        // Based on your actual content structure
        this.linkDatabase = {
            '/notes/20220405102340-psychedelics': {
                title: 'Psychedelics',
                linksTo: [
                    '/notes/20220405103750-doi',
                    '/notes/20220405104217-5_ht2a_receptor',
                    '/notes/20220405104300-5_ht2c_receptor'
                ]
            },
            '/notes/20220405103750-doi': {
                title: 'DOI',
                linksTo: [
                    '/notes/20220405102340-psychedelics',
                    '/notes/20220405104217-5_ht2a_receptor'
                ]
            },
            '/notes/20210518184927-lysergic_acid_diethylamide_lsd': {
                title: 'Lysergic Acid Diethylamide (LSD)',
                linksTo: [
                    '/notes/20220405102340-psychedelics',
                    '/notes/20220405104217-5_ht2a_receptor'
                ]
            },
            '/notes/20230323085540-mdma': {
                title: 'MDMA',
                linksTo: [
                    '/notes/20220405102340-psychedelics'
                ]
            },
            '/notes/20220405104217-5_ht2a_receptor': {
                title: '5-HT2A Receptor',
                linksTo: [
                    '/notes/20220405102340-psychedelics'
                ]
            },
            '/notes/20220405104300-5_ht2c_receptor': {
                title: '5-HT2C Receptor', 
                linksTo: [
                    '/notes/20220405102340-psychedelics'
                ]
            }
        };
    }

    findBacklinksForCurrentPage() {
        const currentPath = this.normalizePath(this.currentPage);
        const backlinks = [];

        // Find pages that link TO the current page
        Object.entries(this.linkDatabase).forEach(([pagePath, pageData]) => {
            if (pageData.linksTo.includes(currentPath)) {
                backlinks.push({
                    title: pageData.title,
                    url: pagePath,
                    context: `References ${document.title.split(' - ')[0] || 'this page'}`
                });
            }
        });

        // Also check for any dynamic links found on the current page
        const outgoingLinks = this.parseCurrentPageLinks();
        outgoingLinks.forEach(link => {
            // This creates bidirectional linking - if current page links to X, 
            // then X should show current page as a backlink
            const targetPageData = this.linkDatabase[link.href];
            if (targetPageData) {
                // console.log(`Current page links to: ${targetPageData.title}`);
            }
        });

        this.backlinks.set(currentPath, backlinks);
        // console.log('Found backlinks for', currentPath, ':', backlinks);
    }


    displayBacklinks() {
        const currentPath = this.normalizePath(this.currentPage);
        const backlinks = this.backlinks.get(currentPath) || [];
        
        // console.log('=== BACKLINKS DEBUG ===');
        // console.log('Current page:', this.currentPage);
        // console.log('Normalized path:', currentPath);
        // console.log('Available backlinks:', backlinks);
        
        // Create backlinks sidebar
        const sidebar = document.createElement('div');
        sidebar.className = 'backlinks-sidebar';
        
        sidebar.innerHTML = `
            <div class="backlinks-container">
                <h3 class="backlinks-title">Backlinks</h3>
                <div class="backlinks-list">
                    ${backlinks.length > 0 ? backlinks.map(link => `
                        <div class="backlink-item">
                            <a href="${link.url}" class="backlink-title">${link.title}</a>
                            <p class="backlink-context">${link.context}</p>
                        </div>
                    `).join('') : ''}
                </div>
            </div>
        `;
        
        // Only show sidebar if there are backlinks
        if (backlinks.length === 0) {
            return;
        }

        // Add CSS styles
        const style = document.createElement('style');
        style.textContent = `
            .backlinks-sidebar {
                position: fixed;
                right: 20px;
                top: 20%;
                width: 300px;
                max-height: 60vh;
                overflow-y: auto;
                background: var(--bg-color, #ffffff);
                border: 1px solid var(--border-color, #ece7de);
                border-radius: 8px;
                box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1);
                z-index: 1000;
                padding: 0;
            }

            .dark .backlinks-sidebar {
                background: var(--bg-card, #1f1c15);
                border-color: var(--border, #2c2820);
                box-shadow: 0 4px 12px rgba(0, 0, 0, 0.3);
            }

            .backlinks-container {
                padding: 16px;
            }

            .backlinks-title {
                margin: 0 0 12px 0;
                font-size: 1.1em;
                font-weight: 600;
                color: var(--text-color, #1a1a1a);
                border-bottom: 1px solid var(--border-color, #ece7de);
                padding-bottom: 8px;
            }

            .dark .backlinks-title {
                color: var(--text, #e8e4dc);
                border-color: var(--border, #2c2820);
            }

            .backlinks-list {
                display: flex;
                flex-direction: column;
                gap: 12px;
            }

            .backlink-item {
                padding: 8px;
                border-radius: 4px;
                background: var(--item-bg, #fdfbf7);
                border-left: 3px solid var(--accent, #b08d57);
            }

            .dark .backlink-item {
                background: var(--border, #2c2820);
            }

            .backlink-title {
                display: block;
                font-weight: 500;
                color: var(--accent, #b08d57);
                text-decoration: none;
                margin-bottom: 4px;
                font-size: 0.9em;
            }

            .backlink-title:hover {
                text-decoration: underline;
            }

            .backlink-context {
                margin: 0;
                font-size: 0.8em;
                color: var(--text-muted, #8a8378);
                line-height: 1.3;
            }

            .dark .backlink-context {
                color: var(--text-muted, #9a9284);
            }

            /* Hide on mobile */
            @media (max-width: 1024px) {
                .backlinks-sidebar {
                    display: none;
                }
            }

            /* Adjust main content margin when backlinks are present */
            .content {
                margin-right: 0;
            }

            @media (min-width: 1400px) {
                .content {
                    margin-right: 320px;
                }
            }
        `;

        document.head.appendChild(style);
        document.body.appendChild(sidebar);
        
        // Add body class to indicate right sidebar is present
        document.body.classList.add('has-right-sidebar');

        // Add scroll behavior
        this.handleScrollBehavior(sidebar);
    }

    handleScrollBehavior(sidebar) {
        let isScrolling = false;
        
        window.addEventListener('scroll', () => {
            if (!isScrolling) {
                sidebar.style.opacity = '0.7';
                isScrolling = true;
            }
            
            clearTimeout(window.scrollTimeout);
            window.scrollTimeout = setTimeout(() => {
                sidebar.style.opacity = '1';
                isScrolling = false;
            }, 150);
        });
    }
}

// Initialize backlinks system when page loads
document.addEventListener('DOMContentLoaded', () => {
    new BacklinksSystem();
});
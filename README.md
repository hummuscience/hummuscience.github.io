# Academic Website System

A Hugo-based academic website with advanced note-taking and publication management features. Built using the hugo-astatine-theme with extensive customizations for knowledge management.

## Quick Start

```bash
# Start development server
hugo server --buildDrafts --port 1314

# Build for production
hugo

# Deploy (customize based on your hosting)
# Example for GitHub Pages or similar
hugo && rsync -av public/ your-server:/path/to/website/
```

## System Overview

This website combines Hugo static site generation with org-roam style knowledge management, featuring:

- **Academic Publications Management** - Add publications via YAML frontmatter
- **Automatic Theme Switching** - Day/night themes based on local sunrise/sunset
- **Backlinks System** - Org-roam style bidirectional linking between notes
- **Smart Search** - Real-time search for notes with keyboard navigation
- **Table of Contents** - Auto-generated TOC for content with multiple headings
- **Auto-Publishing** - Automatically publish linked notes when exporting
- **Centered Layout** - Optimal reading experience with sidebars

## Content Management

### Adding Publications

Edit `content/_index.md` and add to the `publications` array in frontmatter:

```yaml
publications:
  - title: "Your Paper Title"
    authors: ["Author One", "Author Two"]
    journal: "Journal Name"
    year: 2024
    doi: "10.1000/example"
    url: "https://doi.org/10.1000/example"
    pdf: "/path/to/paper.pdf"  # optional
```

Publications are automatically sorted by year (most recent first).

### Managing Notes with ox-hugo

This system is designed to work with [ox-hugo](https://ox-hugo.scripter.co/) for org-mode to Hugo export.

#### Basic Workflow:
1. Write notes in org-mode files in `~/cloud/brain/`
2. Use `#+HUGO_BASE_DIR: ~/cloud/website` in your org files
3. Export with `C-c C-e H H` (ox-hugo export)
4. Auto-publishing will handle linked notes automatically

#### Note Structure:
```org
#+TITLE: Note Title
#+HUGO_BASE_DIR: ~/cloud/website
#+HUGO_SECTION: notes
#+HUGO_DRAFT: false

Your note content with [[id:other-note-id][links to other notes]].
```

### Creating Blog Posts and Projects

Similar to notes, but set `#+HUGO_SECTION: blog` or `#+HUGO_SECTION: projects`.

## Advanced Features

### 1. Automatic Theme Switching

The system automatically switches between light and dark themes based on your location's sunrise/sunset times:

- Uses geolocation API to determine local times
- 24-hour manual override available
- Smooth transitions between themes

### 2. Backlinks System

Shows related pages that link to the current page:

- Appears in right sidebar on notes pages
- Based on actual Hugo `relref` links in content
- Provides context for each backlink

### 3. Smart Search

Real-time search functionality for notes:

- Only appears on notes pages
- Keyboard navigation (arrow keys, Enter, Escape)
- Searches titles and content
- Shows contextual excerpts

### 4. Table of Contents

Auto-generated TOC for content with 2+ headings:

- Appears in left sidebar
- Scroll spy highlights current section
- Collapsible with smooth animations
- Auto-generates heading IDs for linking

### 5. Auto-Publishing Linked Notes

Emacs Lisp function automatically publishes unpublished notes when they're referenced:

- Hooks into ox-hugo export process
- Detects `[[id:note-id][Link Text]]` patterns
- Adds Hugo properties to unpublished notes
- Maintains knowledge graph integrity

## File Structure

```
website/
├── content/
│   ├── _index.md           # Homepage with bio and publications
│   ├── blog/               # Blog posts
│   ├── notes/              # Knowledge base notes
│   └── projects/           # Project pages
├── static/
│   ├── css/
│   │   └── centered-layout.css    # Layout and responsive design
│   └── js/
│       ├── auto-theme.js          # Automatic theme switching
│       ├── backlinks.js           # Backlinks system
│       ├── notes-search.js        # Search functionality
│       └── table-of-contents.js   # TOC generation
├── themes/hugo-astatine-theme/    # Base theme
├── config.toml             # Hugo configuration
├── CLAUDE.md              # AI assistant documentation
└── README.md              # This file
```

## Customization

### Layout Adjustments

Edit `static/css/centered-layout.css`:

- `65ch` max-width for optimal text readability
- Responsive breakpoints at 1200px and 1400px
- Flexbox centering for main content
- Sidebar positioning and widths

### Theme Modifications

The auto-theme system can be customized in `static/js/auto-theme.js`:

- Adjust sunrise/sunset offset times
- Modify geolocation timeout settings
- Change manual override duration

### Search Index

Update `static/js/notes-search.js` to add more notes to the search index:

```javascript
this.searchIndex = [
    {
        title: "Note Title",
        url: "/notes/note-slug/",
        content: "searchable keywords",
        excerpt: "Brief description"
    },
    // Add more notes...
];
```

### Backlinks Database

Modify `static/js/backlinks.js` to add link relationships:

```javascript
this.linkDatabase = {
    '/notes/source-note': {
        title: 'Source Note',
        linksTo: ['/notes/target-note']
    },
    // Add more relationships...
};
```

## Responsive Design

The layout adapts to different screen sizes:

- **≥1400px**: Full layout with TOC left, content center, sidebars right
- **1200-1399px**: Compact layout with smaller sidebars
- **<1200px**: Single column, sidebars hidden
- **<1024px**: Mobile-optimized, TOC hidden
- **<480px**: Search hidden on very small screens

## Keyboard Shortcuts

### Search (on notes pages):
- `Arrow Keys`: Navigate results
- `Enter`: Open selected result
- `Escape`: Close search

### TOC:
- Click header to collapse/expand
- Click items for smooth scroll navigation

## Deployment

The system generates a static site that can be deployed anywhere:

1. **GitHub Pages**: Push to gh-pages branch
2. **Netlify**: Connect repository for automatic builds
3. **Traditional hosting**: Upload `public/` folder contents

### Build Commands:
```bash
# Development
hugo server --buildDrafts --port 1314

# Production
hugo --minify

# With drafts
hugo --buildDrafts --minify
```

## Troubleshooting

### Common Issues:

1. **TOC not showing**: Ensure content has 2+ headings
2. **Backlinks empty**: Check link database in `backlinks.js`
3. **Search not working**: Verify you're on a notes page
4. **Theme not switching**: Check browser geolocation permissions
5. **Images too large**: CSS handles various image types automatically

### Debug Mode:

Uncomment console.log statements in JavaScript files for debugging:

```javascript
// In backlinks.js
console.log('Found backlinks:', backlinks);

// In auto-theme.js  
console.log('Current time vs sunrise/sunset:', times);
```

## Performance

The system is optimized for performance:

- Static site generation (fast loading)
- CSS and JS minification
- Responsive images
- Efficient search indexing
- Minimal external dependencies

## Browser Support

Modern browsers with support for:
- ES6 JavaScript features
- CSS Flexbox
- Geolocation API
- Local Storage
- CSS Custom Properties

---

Built with Hugo, enhanced with custom JavaScript and CSS for an optimal academic writing and knowledge management experience.
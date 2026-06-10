# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Personal academic website for a neuroscientist, built with Hugo. Content is authored in Org-mode (`~/cloud/brain/`) and exported to Hugo markdown via [ox-hugo](https://ox-hugo.scripter.co/). The site showcases publications, blog posts, and interconnected research notes with org-roam style backlinks.

## Common Commands

```bash
# Development server
hugo server                  # serve locally with live reload
hugo server -D -F            # include drafts and future posts

# Production build
hugo                         # build to /public/
rm -rf public && hugo        # clean rebuild
```

## Architecture

### Configuration

- **Active config**: `config.toml` — theme list is `["osm"]` only
- **Base URL**: `http://cumol.org/`
- **Markup**: `goldmark.renderer.unsafe = true` — raw HTML allowed in markdown

### Themes & Layouts

- **All page templates are custom**, in `/layouts/` (baseof, index, list,
  single, terms, taxonomy, 404 + partials head/header/footer/extra_js/katex/page-list).
  There is no full theme dependency and no Tailwind/npm build step. The old
  `hugo-astatine-theme` directory is still on disk but unused.
- **osm** (component, git submodule): provides only the
  `{{< openstreetmap mapName="..." >}}` shortcode.
- **Styling**: single hand-written stylesheet `/static/css/site.css`
  ("Refined Academic" design — warm paper light mode, warm dark mode, Lora +
  Inter self-hosted in `/static/fonts/`). All colors are CSS custom
  properties; dark mode = `.dark` class on `<html>` flipping tokens.
  Design spec: `docs/superpowers/specs/2026-06-10-website-redesign-design.md`.

### Content Structure

| Path | Purpose |
|------|---------|
| `content/_index.md` | Homepage: bio + publications array in YAML frontmatter |
| `content/blog/` | Blog posts (e.g., `20250112201223-hummus.md`) |
| `content/notes/` | Research notes with org-roam timestamp IDs (e.g., `20220405102340-psychedelics.md`) |
| `static/img/` | Manual images |
| `static/ox-hugo/` | Auto-exported images from org-mode |

### Content Authoring Workflow

Content is written in org-mode and exported via ox-hugo (`C-c C-e H H`). An Emacs package (`auto-publish-linked-notes.el`) auto-publishes linked but unpublished notes during export. Direct markdown creation: `hugo new blog/post-name.md` or `hugo new notes/note-name.md`.

### Publication Format

Publications in `content/_index.md` use this exact YAML structure:
```yaml
publications:
  - authors:
        - name: "Last, F."
        - name: "Your Name"
          me: true              # flags your name for highlighting
    title: "Paper Title"
    date: 2025                  # year only
    journal: "Journal Name"
    image: img/paper-figure.png # thumbnail in /static/img/
    citation: "@article{...}"   # BibTeX string
    pdf: https://doi.org/...    # link to paper
    links:                      # additional links (optional)
      - name: "Preprint"
        url: "https://..."
    description: "Plain text summary of the paper."
```

Note: `authors` is an array of objects `{name, me?}`, not plain strings. The `me: true` flag marks the site owner's name.

## Custom JavaScript Systems

All loaded in `/layouts/partials/extra_js.html`. All are standalone with no cross-dependencies.

1. **`/static/js/auto-theme.js`** — Day/night switching based on geolocation sunrise/sunset. Overrides the theme's built-in `common.js` `toggleMode()`/`updateMode()`. Uses `localStorage` key `theme-manual-override` with 24h expiry. Rechecks every 60s.

2. **`/static/js/backlinks.js`** — Right sidebar showing pages that link to current page. **Has a hardcoded link database** (lines ~87-128) mapping note paths to their outgoing links — must be manually updated when notes are added.

3. **`/static/js/notes-search.js`** — Search box on `/notes/` pages. **Has a hardcoded search index** (lines ~20-57) — must be manually updated when notes are added. Keyboard nav: arrows, Enter, Escape. Max 8 results, min 2 chars.

4. **`/static/js/table-of-contents.js`** — Left sidebar TOC with scroll spy. Activates on pages with 2+ headings. Auto-generates heading IDs. Active on `/blog/`, `/notes/`, `/projects/` paths.

### Important: Hardcoded Data

Both `backlinks.js` and `notes-search.js` contain hardcoded databases of the 6 current notes. **When adding new notes, both files must be updated manually** with the new note's URL, title, content keywords, and link relationships.

### CSS Classes Set by JavaScript
- `has-toc` — TOC sidebar is present (adjusts left margin)
- `has-right-sidebar` — backlinks or search active (adjusts right margin)
- `dark` — dark theme active

## Responsive Layout

Content is a centered 720px column inside `<main>`. The JS-injected sidebars
(fixed-position) coexist via body classes:

- **≥1500px**: centered content, sidebars float free
- **1200–1499px**: `body.has-toc` adds left margin, `body.has-right-sidebar` adds right margin
- **<1200px**: site.css force-hides `.toc-sidebar`, `.backlinks-sidebar`, `.notes-search-container`
- **<768px**: hamburger nav (`toggleMenu()` toggles `.hidden` on `#navbar-default`); publication cards and hero stack vertically

## Debugging

Uncomment `console.log` statements in the JS files to enable debug output. Each file has commented-out logging for its key operations.

# Website Redesign — Design Spec

**Date:** 2026-06-10
**Status:** Approved by user (visual direction, typography, and architecture validated via visual companion mockups in `.superpowers/brainstorm/759719-1781080785/`)

## Goal

Make cumol.org look professional and current. Full-site redesign: homepage, blog, notes, taxonomy pages, plus a new About page. Replace the `hugo-astatine-theme` with custom lightweight layouts owned by this repo.

## Visual Design ("Refined Academic")

Validated mockup: `final-design.html` in the brainstorm session directory.

### Color system (CSS custom properties)

| Token | Light | Dark |
|-------|-------|------|
| `--bg` | `#fdfbf7` (warm paper) | `#16140f` (warm near-black) |
| `--bg-card` | `#ffffff` | `#1f1c15` |
| `--text` | `#1a1a1a` | `#e8e4dc` |
| `--text-heading` | `#1a1a1a` | `#f0ece4` |
| `--text-muted` | `#8a8378` | `#9a9284` |
| `--text-body` | `#44403a` | `#bdb6a9` |
| `--accent` | `#b08d57` (muted gold) | `#c9a36a` |
| `--border` | `#ece7de` | `#2c2820` |
| `--thumb-bg` | `#e8e2d8` | `#353026` |

Dark mode = `.dark` class on `<html>`/`<body>` flipping the custom properties. No separate dark stylesheet.

### Typography

- **Headings, site title, publication titles:** Lora (serif), weights 400–700 + italic.
- **Body, nav, metadata, UI:** Inter (sans), weights 400–700.
- **Fonts are self-hosted** in `/static/fonts/` as woff2 with `@font-face` declarations and `font-display: swap`. No Google Fonts CDN (GDPR — German court precedent on Google Fonts embedding).
- Body text ≥16px, line-height ~1.7, content measure ~65ch.
- Small-caps-style labels (e.g. "NEUROSCIENTIST"): Inter, uppercase, letter-spacing ~0.15em, accent color.

### Key components

- **Header:** site name in Lora left; nav right in Inter uppercase small labels: Blog · Notes · About · theme toggle (moon/sun). Bottom border in `--border`. Tags/Categories leave the main nav (still reachable from listing pages and post metadata).
- **Homepage hero:** circular avatar (`/img/main.jpg`, 78–96px, subtle border + shadow), name in Lora, "Neuroscientist" label in accent, bio paragraph in Inter.
- **Publication cards:** white/dark card, 1px border, 10px radius, subtle shadow (light only). Row layout: thumbnail left (fixed width, rounded), then title (Lora, semibold), authors/journal/year in muted Inter (site owner's name highlighted via existing `me: true` flag), link row (PDF · Cite · Preprint) in accent semibold. Cards stack to column layout <768px. Cite toggles the BibTeX block (existing behavior preserved).
- **Blog listing:** each entry shows title (Lora), date, and a one-line summary (Hugo `.Summary` or `description` frontmatter) — not just a bare title line.
- **Notes listing:** same list treatment; search box restyled to match.

## Architecture

### Removed

- `hugo-astatine-theme` from the theme list in `config.toml` (theme directory may remain on disk but is unused).
- Tailwind build step (npm) — no longer needed.
- `/static/css/centered-layout.css` — superseded by the new stylesheet.

### Kept

- `osm` theme (git submodule) — only provides the `{{< openstreetmap >}}` shortcode.
- Publication YAML format in `content/_index.md` frontmatter — unchanged.
- All four custom JS systems (see below).
- Existing content files, permalinks, RSS, and the committed `public/` deployment model.

### New layouts (in `/layouts/`)

| File | Purpose |
|------|---------|
| `_default/baseof.html` | Base: header, footer, CSS/JS includes |
| `index.html` | Homepage: bio + publications from frontmatter |
| `_default/list.html` | Blog/notes listings (title, date, summary) |
| `_default/single.html` | Blog posts and notes |
| `_default/terms.html` | Tags/categories term lists |
| `_default/taxonomy.html` | Pages for a single tag/category |
| `404.html` | Not-found page |
| `partials/head.html` | Rewritten: meta/SEO + new CSS (replaces current override) |
| `partials/extra_js.html` | Kept: loads custom JS, conditional KaTeX |

New stylesheet: `/static/css/site.css` (single file, custom properties, no framework).

### Custom JS integration

All four files keep working; only class hooks/colors are updated where needed:

1. `auto-theme.js` — unchanged logic (geolocation sunrise/sunset, manual override with 24h expiry). The inline `common.js` from the old theme is replaced by a small inline script in `head.html` that applies the stored theme class before first paint (no flash).
2. `backlinks.js` — restyled sidebar; hardcoded link DB unchanged (still requires manual updates when notes are added).
3. `notes-search.js` — restyled; hardcoded index unchanged.
4. `table-of-contents.js` — restyled; same activation rules (2+ headings, blog/notes/projects paths).

Responsive behavior preserved: three-column ≥1400px (TOC left, 65ch content, sidebars right), compact 1200–1399px, single column <1200px, TOC hidden <1024px, search hidden <480px, publication cards stack <768px.

## Content Changes

- **New:** `content/about.md` — About page (longer bio, contact, CV link placeholder), fixing the dead `/about/` nav link. Stubbed from existing bio; user extends later.
- **Blog summaries:** add `description` frontmatter (or rely on `.Summary`) so listings show one-line summaries.

## Cleanup

Delete from repo:

- `config.yml`, `config_ananke.toml`, `config_old.toml` (inactive theme configs)
- `content/_index_backup.md`, `public/_index_backup/`
- Emacs lock/backup files: `content/muad@shulgin.13398:1694962527`, `content/blog/20250112201223-hummus.md~`, the deleted `content/#_index.md#` / `content/.#_index.md` entries
- Stale screenshots in repo root (`blog-listing.png`, `blog-post*.png`, `homepage-*.png`) and `.playwright-mcp/`

Add `.gitignore` for: `.superpowers/`, emacs artifacts (`*~`, `\#*\#`, `.\#*`), `.playwright-mcp/`. (`public/` stays committed — it is the deployment mechanism.)

## Verification

- `hugo` builds with no errors; clean rebuild (`rm -rf public && hugo`) produces all pages.
- Visual check (browser) of: homepage light + dark, blog listing, a blog post with TOC, a note with backlinks + search, a tag page, About, 404.
- Dark-mode toggle and auto-switching still function; no flash of wrong theme on load.
- Map shortcode page still renders (osm).
- KaTeX conditional loading still works on a `KaTeX: true` page.

## Out of Scope

- Generating the backlinks/search indexes automatically from content (stays hardcoded; candidate for a future improvement).
- New content beyond the About stub and blog summaries.
- Changing hosting/deployment.

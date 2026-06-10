# Website Redesign Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the hugo-astatine-theme with custom lightweight layouts implementing the approved "Refined Academic" design (spec: `docs/superpowers/specs/2026-06-10-website-redesign-design.md`).

**Architecture:** Custom Hugo layouts in `/layouts/` + one hand-written stylesheet (`/static/css/site.css`) with CSS custom properties for light/dark theming. Self-hosted Lora + Inter fonts. The `osm` theme stays (map shortcode only). The four custom JS systems are kept; their injected styles are re-pointed at the new color tokens.

**Tech Stack:** Hugo v0.92.2 (extended) — note this is OLD; avoid template functions newer than 0.92. No Node/Tailwind. Plain CSS + vanilla JS.

**Verification model:** This is a static site with no test framework. Each task's "test" is `hugo` building cleanly plus grep checks of generated output in `/public/`. Full builds only work from Task 6 onward (Tasks 2–5 create templates while the config still points at the old theme — do not build in between; the new partials would mix with old theme templates).

**Key facts an engineer needs (verified against the codebase):**

- Hugo is v0.92.2. `hugo` builds to `/public/` which is COMMITTED (it is the deployment mechanism). `paginate`, `pygmentsCodefences`, `pygmentsStyle = "native"` (inline chroma styles, dark code blocks in both modes — intended).
- `auto-theme.js` needs: a button matching `[onclick="toggleMode()"]` (its optional `svg use` icon update is a no-op with our markup — fine), and `toggleMenu()` needs `<div id="navbar-default">` whose `hidden` class gets toggled.
- The inline theme-init script in `layouts/partials/head.html` (lines 88–115) sets `.dark` on `<html>` before paint. Keep it verbatim.
- `backlinks.js` scans `main a[href^="/"]`; `table-of-contents.js` looks for `main, .content, article`; `notes-search.js` activates when `location.pathname` includes `/notes`. So the content wrapper MUST be `<main>`.
- All three sidebar JS files inject their own `<style>` blocks (with their own media queries) — site.css does not need to style sidebar internals, only provide color tokens + margin adjustments + a kill-switch below 1200px.
- `notes-search.js` uses a `.hidden` class on its results dropdown → site.css must define `.hidden { display: none; }`.
- Publications data lives in `content/_index.md` YAML frontmatter: `name`, `imgname.{name,alt}`, `personal_title`, `publications[]` with `authors[].{name,me}`, `title`, `date` (year int), `journal`, `image` (e.g. `img/kamm2025.png`, needs leading `/` when rendered), `citation` (BibTeX string), `pdf`, `links[].{name,url}`, `description`.
- Content frontmatter is TOML (`+++`). Notes have NO `date`, only `lastmod` → `.PublishDate` is zero for notes; use `.Lastmod` as fallback.
- `extra_js.html` currently references theme partials `katex.html` and `home_modal.html`. KaTeX partial must be copied into `/layouts/partials/`; the home modal is dropped (the new design has no homepage modal).

---

### Task 1: Self-host Lora + Inter fonts

**Files:**
- Create: `static/fonts/lora-400.woff2`, `static/fonts/lora-400i.woff2`, `static/fonts/lora-600.woff2`, `static/fonts/lora-700.woff2`, `static/fonts/inter-400.woff2`, `static/fonts/inter-500.woff2`, `static/fonts/inter-600.woff2`, `static/fonts/inter-700.woff2`

- [ ] **Step 1: Download woff2 files via google-webfonts-helper**

```bash
mkdir -p /home/muad/cloud/website/static/fonts
cd /home/muad/cloud/website/static/fonts
curl -sL -o lora.zip "https://gwfh.mranftl.com/api/fonts/lora?download=zip&subsets=latin&variants=regular,italic,600,700&formats=woff2"
unzip -o lora.zip && rm lora.zip
curl -sL -o inter.zip "https://gwfh.mranftl.com/api/fonts/inter?download=zip&subsets=latin&variants=regular,500,600,700&formats=woff2"
unzip -o inter.zip && rm inter.zip
```

Expected: 8 `.woff2` files with versioned names like `lora-v36-latin-regular.woff2`.

Fallback if gwfh.mranftl.com is down: download TTFs from `https://github.com/google/fonts/tree/main/ofl/lora` and `https://github.com/rsms/inter/releases` (use `InterVariable.woff2` from the release zip) and adjust the `@font-face` blocks in Task 2 accordingly.

- [ ] **Step 2: Rename to stable names**

```bash
cd /home/muad/cloud/website/static/fonts
mv lora-*-latin-regular.woff2 lora-400.woff2
mv lora-*-latin-italic.woff2 lora-400i.woff2
mv lora-*-latin-600.woff2 lora-600.woff2
mv lora-*-latin-700.woff2 lora-700.woff2
mv inter-*-latin-regular.woff2 inter-400.woff2
mv inter-*-latin-500.woff2 inter-500.woff2
mv inter-*-latin-600.woff2 inter-600.woff2
mv inter-*-latin-700.woff2 inter-700.woff2
ls
```

Expected: exactly `inter-400.woff2 inter-500.woff2 inter-600.woff2 inter-700.woff2 lora-400.woff2 lora-400i.woff2 lora-600.woff2 lora-700.woff2`.

- [ ] **Step 3: Sanity-check the files are real woff2**

```bash
file /home/muad/cloud/website/static/fonts/*.woff2 | head -2
```

Expected: each reports `Web Open Font Format (Version 2)`.

- [ ] **Step 4: Commit**

```bash
cd /home/muad/cloud/website
git add static/fonts
git commit -m "Add self-hosted Lora and Inter woff2 fonts"
```

---

### Task 2: Write the new stylesheet

**Files:**
- Create: `static/css/site.css`

- [ ] **Step 1: Create `static/css/site.css` with exactly this content**

```css
/* ==========================================================================
   site.css — Refined Academic design
   Single stylesheet for cumol.org. Light/dark via CSS custom properties;
   the .dark class on <html> flips the tokens (set by auto-theme.js and the
   inline head script). No framework.
   ========================================================================== */

/* --- Fonts (self-hosted, GDPR-safe) --- */
@font-face {
  font-family: 'Lora'; font-style: normal; font-weight: 400;
  font-display: swap; src: url('/fonts/lora-400.woff2') format('woff2');
}
@font-face {
  font-family: 'Lora'; font-style: italic; font-weight: 400;
  font-display: swap; src: url('/fonts/lora-400i.woff2') format('woff2');
}
@font-face {
  font-family: 'Lora'; font-style: normal; font-weight: 600;
  font-display: swap; src: url('/fonts/lora-600.woff2') format('woff2');
}
@font-face {
  font-family: 'Lora'; font-style: normal; font-weight: 700;
  font-display: swap; src: url('/fonts/lora-700.woff2') format('woff2');
}
@font-face {
  font-family: 'Inter'; font-style: normal; font-weight: 400;
  font-display: swap; src: url('/fonts/inter-400.woff2') format('woff2');
}
@font-face {
  font-family: 'Inter'; font-style: normal; font-weight: 500;
  font-display: swap; src: url('/fonts/inter-500.woff2') format('woff2');
}
@font-face {
  font-family: 'Inter'; font-style: normal; font-weight: 600;
  font-display: swap; src: url('/fonts/inter-600.woff2') format('woff2');
}
@font-face {
  font-family: 'Inter'; font-style: normal; font-weight: 700;
  font-display: swap; src: url('/fonts/inter-700.woff2') format('woff2');
}

/* --- Design tokens --- */
:root {
  --bg: #fdfbf7;
  --bg-card: #ffffff;
  --text: #1a1a1a;
  --text-heading: #1a1a1a;
  --text-body: #44403a;
  --text-muted: #8a8378;
  --accent: #b08d57;
  --border: #ece7de;
  --thumb-bg: #e8e2d8;
  --shadow: 0 1px 4px rgba(0, 0, 0, 0.04);

  /* Legacy aliases — the injected styles in backlinks.js / notes-search.js /
     table-of-contents.js reference these names. Do not remove. */
  --bg-color: var(--bg-card);
  --border-color: var(--border);
  --text-color: var(--text);
  --item-bg: var(--bg);

  --font-serif: 'Lora', Georgia, serif;
  --font-sans: 'Inter', -apple-system, 'Segoe UI', Helvetica, Arial, sans-serif;
  --font-mono: ui-monospace, 'JetBrains Mono', 'Fira Code', Menlo, monospace;
}

.dark {
  --bg: #16140f;
  --bg-card: #1f1c15;
  --text: #e8e4dc;
  --text-heading: #f0ece4;
  --text-body: #bdb6a9;
  --text-muted: #9a9284;
  --accent: #c9a36a;
  --border: #2c2820;
  --thumb-bg: #353026;
  --shadow: none;
}

/* --- Base --- */
*, *::before, *::after { box-sizing: border-box; }

html { -webkit-text-size-adjust: 100%; scroll-behavior: smooth; }

body {
  margin: 0;
  background: var(--bg);
  color: var(--text-body);
  font-family: var(--font-sans);
  font-size: 17px;
  line-height: 1.7;
  transition: background-color 0.3s ease, color 0.3s ease;
}

.hidden { display: none; }

h1, h2, h3, h4, h5, h6 {
  font-family: var(--font-serif);
  color: var(--text-heading);
  line-height: 1.3;
  margin: 2em 0 0.6em;
}
h1 { font-size: 2rem; font-weight: 600; }
h2 { font-size: 1.5rem; font-weight: 600; }
h3 { font-size: 1.2rem; font-weight: 600; }
h4, h5, h6 { font-size: 1.05rem; font-weight: 600; }

a {
  color: var(--accent);
  text-decoration: none;
  border-bottom: 1px solid transparent;
  transition: border-color 0.15s ease, color 0.15s ease;
}
a:hover { border-bottom-color: var(--accent); }

strong, b { color: var(--text-heading); }

hr { border: none; border-top: 1px solid var(--border); margin: 2.5rem 0; }

img { max-width: 100%; height: auto; border-radius: 6px; }

blockquote {
  margin: 1.5rem 0;
  padding: 0.25rem 1.25rem;
  border-left: 3px solid var(--accent);
  color: var(--text-muted);
}
blockquote p { margin: 0.5rem 0; }

code {
  font-family: var(--font-mono);
  font-size: 0.85em;
  background: var(--thumb-bg);
  color: var(--text-heading);
  padding: 0.15em 0.4em;
  border-radius: 4px;
}

/* Chroma highlight blocks carry inline colors (pygmentsStyle=native, dark).
   Give them a consistent shell and undo the inline-code chip inside. */
pre {
  overflow-x: auto;
  padding: 1rem 1.25rem;
  border-radius: 8px;
  background: #202020;
  line-height: 1.5;
}
pre code { background: none; padding: 0; color: inherit; font-size: 0.85rem; }
.highlight pre { margin: 1.5rem 0; }

table {
  border-collapse: collapse;
  width: 100%;
  margin: 1.5rem 0;
  font-size: 0.92em;
}
th, td { border: 1px solid var(--border); padding: 0.5rem 0.75rem; text-align: left; }
th { font-family: var(--font-sans); font-weight: 600; color: var(--text-heading); background: var(--thumb-bg); }

figure { margin: 1.5rem 0; }
figcaption { font-size: 0.85rem; color: var(--text-muted); margin-top: 0.5rem; }

::selection { background: var(--accent); color: #fff; }

/* --- Layout --- */
main {
  max-width: 720px;
  margin: 0 auto;
  padding: 1rem 1.25rem 4rem;
}

/* Sidebar coexistence: JS adds these body classes when it injects sidebars.
   Sidebars are fixed-position (styles injected by the JS files themselves).
   Between 1200–1499px the centered column would collide with them. */
@media (min-width: 1200px) and (max-width: 1499px) {
  body.has-right-sidebar main { margin-right: 360px; }
  body.has-toc main { margin-left: 340px; }
}
/* Below 1200px: single column, no floating panels (JS media queries only
   kick in at 1024px, so enforce the 1200px cutoff here). */
@media (max-width: 1199px) {
  .backlinks-sidebar, .notes-search-container, .toc-sidebar {
    display: none !important;
  }
}

/* --- Header / nav --- */
.site-header {
  border-bottom: 1px solid var(--border);
  background: var(--bg);
}
.site-header-inner {
  max-width: 980px;
  margin: 0 auto;
  padding: 1rem 1.25rem;
  display: flex;
  flex-wrap: wrap;
  justify-content: space-between;
  align-items: center;
}
.site-title {
  font-family: var(--font-serif);
  font-size: 1.15rem;
  font-weight: 600;
  color: var(--text-heading);
}
.site-title:hover { border-bottom-color: transparent; color: var(--accent); }

.nav-toggle {
  display: none;
  background: none;
  border: none;
  color: var(--text-muted);
  cursor: pointer;
  padding: 0.25rem;
}

#navbar-default ul {
  display: flex;
  align-items: center;
  gap: 1.5rem;
  list-style: none;
  margin: 0;
  padding: 0;
}
#navbar-default a {
  font-size: 0.78rem;
  font-weight: 500;
  letter-spacing: 0.1em;
  text-transform: uppercase;
  color: var(--text-muted);
}
#navbar-default a:hover { color: var(--accent); border-bottom-color: var(--accent); }

.theme-toggle {
  background: none;
  border: none;
  cursor: pointer;
  color: var(--text-muted);
  padding: 0.25rem;
  display: inline-flex;
  align-items: center;
}
.theme-toggle:hover { color: var(--accent); }
.icon-sun { display: none; }
.dark .icon-sun { display: inline; }
.dark .icon-moon { display: none; }

@media (max-width: 767px) {
  .nav-toggle { display: inline-flex; }
  #navbar-default { width: 100%; }
  #navbar-default.hidden { display: none; }
  #navbar-default ul {
    flex-direction: column;
    align-items: flex-start;
    gap: 0;
    padding: 0.5rem 0;
  }
  #navbar-default li { width: 100%; border-bottom: 1px solid var(--border); }
  #navbar-default li:last-child { border-bottom: none; }
  #navbar-default a { display: block; padding: 0.75rem 0; }
}
@media (min-width: 768px) {
  #navbar-default { display: block !important; }
}

/* --- Footer --- */
.site-footer {
  border-top: 1px solid var(--border);
  margin-top: 2rem;
}
.site-footer-inner {
  max-width: 980px;
  margin: 0 auto;
  padding: 1.25rem;
  font-size: 0.8rem;
  color: var(--text-muted);
  display: flex;
  justify-content: space-between;
  flex-wrap: wrap;
  gap: 0.5rem;
}

/* --- Homepage hero --- */
.hero {
  display: flex;
  align-items: center;
  gap: 1.5rem;
  margin-top: 2.5rem;
}
.hero-avatar {
  width: 96px;
  height: 96px;
  border-radius: 50%;
  object-fit: cover;
  border: 3px solid var(--bg-card);
  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.12);
  flex-shrink: 0;
}
.hero-name {
  font-size: 2.1rem;
  margin: 0;
}
.hero-title {
  font-family: var(--font-sans);
  font-size: 0.72rem;
  font-weight: 600;
  letter-spacing: 0.16em;
  text-transform: uppercase;
  color: var(--accent);
  margin: 0.4rem 0 0;
}
.hero-bio { margin-top: 1.25rem; }
.hero-bio p { margin: 0.9rem 0; }

/* --- Section heading (Publications) --- */
.section-heading {
  margin-top: 3rem;
  padding-bottom: 0.5rem;
  border-bottom: 1px solid var(--border);
}

/* --- Publication cards --- */
.publications { display: flex; flex-direction: column; gap: 1rem; margin-top: 1.25rem; }

.pub-card {
  display: flex;
  gap: 1.25rem;
  background: var(--bg-card);
  border: 1px solid var(--border);
  border-radius: 10px;
  padding: 1.25rem;
  box-shadow: var(--shadow);
  transition: border-color 0.15s ease;
}
.pub-card:hover { border-color: var(--accent); }

.pub-thumb { flex: 0 0 120px; }
.pub-thumb img {
  width: 120px;
  height: 90px;
  object-fit: cover;
  border-radius: 6px;
  background: var(--thumb-bg);
}

.pub-body { min-width: 0; }
.pub-title {
  font-size: 1.05rem;
  margin: 0 0 0.4rem;
  line-height: 1.45;
}
.pub-title a { color: var(--text-heading); }
.pub-title a:hover { color: var(--accent); border-bottom-color: transparent; }

.pub-meta {
  font-size: 0.82rem;
  color: var(--text-muted);
  margin: 0 0 0.5rem;
}
.pub-me { color: var(--text-heading); font-weight: 600; }

.pub-description {
  font-size: 0.88rem;
  line-height: 1.6;
  margin: 0 0 0.6rem;
}

.pub-links {
  display: flex;
  flex-wrap: wrap;
  align-items: baseline;
  gap: 1rem;
  font-size: 0.8rem;
}
.pub-links a, .pub-cite summary {
  font-weight: 600;
  letter-spacing: 0.04em;
  text-transform: uppercase;
  color: var(--accent);
  cursor: pointer;
}
.pub-cite { display: inline; }
.pub-cite[open] { flex-basis: 100%; }
.pub-cite summary { list-style: none; }
.pub-cite summary::-webkit-details-marker { display: none; }
.pub-cite pre {
  margin-top: 0.75rem;
  font-size: 0.72rem;
  white-space: pre-wrap;
  word-break: break-word;
}

@media (max-width: 767px) {
  .pub-card { flex-direction: column; }
  .pub-thumb img { width: 100%; height: auto; max-height: 180px; }
  .hero { flex-direction: column; text-align: center; }
}

/* --- Listing pages (blog, notes, term pages) --- */
.page-header h1 { margin-top: 2rem; }

.post-list { list-style: none; margin: 1.5rem 0 0; padding: 0; }
.post-item { padding: 1.1rem 0; border-bottom: 1px solid var(--border); }
.post-item:first-child { border-top: 1px solid var(--border); }

.post-link {
  display: flex;
  justify-content: space-between;
  align-items: baseline;
  gap: 1rem;
  border-bottom: none;
}
.post-title {
  font-family: var(--font-serif);
  font-size: 1.1rem;
  font-weight: 600;
  color: var(--text-heading);
}
.post-link:hover .post-title { color: var(--accent); }
.post-date {
  font-size: 0.78rem;
  color: var(--text-muted);
  white-space: nowrap;
}
.post-summary {
  font-size: 0.88rem;
  color: var(--text-muted);
  margin: 0.35rem 0 0;
}

/* --- Single article --- */
.article-title { margin-top: 2rem; margin-bottom: 0.4rem; }
.article-meta {
  font-size: 0.82rem;
  color: var(--text-muted);
  margin-bottom: 2rem;
}
.article-content { overflow-wrap: break-word; }

.article-footer { margin-top: 3rem; }
.term-list { display: flex; flex-wrap: wrap; gap: 0.5rem; margin-top: 0.75rem; }
.term-chip {
  font-size: 0.75rem;
  font-weight: 500;
  background: var(--thumb-bg);
  color: var(--text-body);
  border-radius: 999px;
  padding: 0.2rem 0.7rem;
}
.term-chip:hover { color: var(--accent); border-bottom-color: transparent; }
.term-count { color: var(--text-muted); margin-left: 0.25rem; }

.term-cloud { list-style: none; padding: 0; margin: 1.5rem 0; display: flex; flex-wrap: wrap; gap: 0.6rem; }

/* --- 404 --- */
.not-found { text-align: center; margin-top: 4rem; }
.not-found h1 { font-size: 4rem; margin-bottom: 0; }
```

- [ ] **Step 2: Commit**

```bash
cd /home/muad/cloud/website
git add static/css/site.css
git commit -m "Add site.css: Refined Academic design system"
```

---

### Task 3: New header, footer, head, and JS-loader partials

**Files:**
- Create: `layouts/partials/header.html`
- Create: `layouts/partials/footer.html`
- Create: `layouts/partials/katex.html` (copy from theme)
- Modify: `layouts/partials/head.html` (replace fonts/CSS section, lines 71–87)
- Modify: `layouts/partials/extra_js.html` (drop home_modal)

- [ ] **Step 1: Create `layouts/partials/header.html`**

The button MUST keep `onclick="toggleMode()"`, the menu container MUST keep `id="navbar-default"` and start with class `hidden`, and the hamburger MUST keep `onclick="toggleMenu()"` — auto-theme.js depends on these exact hooks.

```html
<header class="site-header">
  <div class="site-header-inner">
    <a class="site-title" href="{{ .Site.BaseURL }}">{{ .Site.Title }}</a>
    <button type="button" class="nav-toggle" onclick="toggleMenu()"
            aria-controls="navbar-default" aria-expanded="false" aria-label="Open main menu">
      <svg width="22" height="22" viewBox="0 0 24 24" fill="none" stroke="currentColor"
           stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">
        <line x1="3" y1="6" x2="21" y2="6"></line>
        <line x1="3" y1="12" x2="21" y2="12"></line>
        <line x1="3" y1="18" x2="21" y2="18"></line>
      </svg>
    </button>
    <nav id="navbar-default" class="hidden">
      <ul>
        {{ range sort .Site.Menus.main "Weight" }}
        <li><a href="{{ .URL }}">{{ .Name }}</a></li>
        {{ end }}
        <li>
          <button type="button" onclick="toggleMode()" class="theme-toggle"
                  aria-label="Toggle between dark and light mode">
            <svg class="icon-moon" width="18" height="18" viewBox="0 0 24 24" fill="none"
                 stroke="currentColor" stroke-width="2" stroke-linecap="round"
                 stroke-linejoin="round" aria-hidden="true">
              <path d="M21 12.79A9 9 0 1 1 11.21 3 7 7 0 0 0 21 12.79z"></path>
            </svg>
            <svg class="icon-sun" width="18" height="18" viewBox="0 0 24 24" fill="none"
                 stroke="currentColor" stroke-width="2" stroke-linecap="round"
                 stroke-linejoin="round" aria-hidden="true">
              <circle cx="12" cy="12" r="5"></circle>
              <line x1="12" y1="1" x2="12" y2="3"></line>
              <line x1="12" y1="21" x2="12" y2="23"></line>
              <line x1="4.22" y1="4.22" x2="5.64" y2="5.64"></line>
              <line x1="18.36" y1="18.36" x2="19.78" y2="19.78"></line>
              <line x1="1" y1="12" x2="3" y2="12"></line>
              <line x1="21" y1="12" x2="23" y2="12"></line>
              <line x1="4.22" y1="19.78" x2="5.64" y2="18.36"></line>
              <line x1="18.36" y1="5.64" x2="19.78" y2="4.22"></line>
            </svg>
          </button>
        </li>
      </ul>
    </nav>
  </div>
</header>
```

- [ ] **Step 2: Create `layouts/partials/footer.html`**

```html
<footer class="site-footer">
  <div class="site-footer-inner">
    <span>© {{ now.Format "2006" }} {{ .Site.Params.authorName }}</span>
    <span>
      {{ if .Site.Params.blogrss }}<a href="{{ .Site.BaseURL }}index.xml">RSS</a> · {{ end }}
      Built with <a href="https://gohugo.io">Hugo</a>
    </span>
  </div>
</footer>
```

- [ ] **Step 3: Copy the KaTeX partial out of the theme**

```bash
cp /home/muad/cloud/website/themes/hugo-astatine-theme/layouts/partials/katex.html /home/muad/cloud/website/layouts/partials/katex.html
```

- [ ] **Step 4: Update `layouts/partials/head.html` — replace the fonts + CSS block**

In `layouts/partials/head.html`, replace lines 71–79 (from `<!-- Fonts: preconnect for speed -->` through the centered-layout link) — i.e. this block:

```html
<!-- Fonts: preconnect for speed -->
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=Instrument+Serif:ital@0;1&family=JetBrains+Mono:wght@400;500&family=DM+Sans:ital,opsz,wght@0,9..40,300;0,9..40,400;0,9..40,500;0,9..40,600;1,9..40,400&display=swap" rel="stylesheet">
<!-- Style configuration -->
{{ $style := resources.Get "css/style.css" | resources.Minify }}
<link rel="stylesheet" href="{{ $style.Permalink }}">
<!-- Centered layout CSS -->
<link rel="stylesheet" href="/css/centered-layout.css">
```

with:

```html
<!-- Self-hosted fonts (preload the two weights used above the fold) -->
<link rel="preload" href="/fonts/lora-600.woff2" as="font" type="font/woff2" crossorigin>
<link rel="preload" href="/fonts/inter-400.woff2" as="font" type="font/woff2" crossorigin>
<!-- Site stylesheet -->
<link rel="stylesheet" href="/css/site.css">
```

Then change `<meta name="theme-color" content="#faf9f7">` to `<meta name="theme-color" content="#fdfbf7">`.

Keep EVERYTHING else in head.html unchanged — especially the SEO blocks (lines 1–70) and the inline theme-init script + `.no-transitions` style at the bottom (lines 88–115).

- [ ] **Step 5: Update `layouts/partials/extra_js.html`** — remove the home_modal include (last 3 lines). Full new content:

```html
{{/* Custom auto-theme JavaScript */}}
<script src="/js/auto-theme.js"></script>

{{/* Backlinks system for org-roam content */}}
<script src="/js/backlinks.js"></script>

{{/* Notes search functionality */}}
<script src="/js/notes-search.js"></script>

{{/* Table of contents for content pages */}}
<script src="/js/table-of-contents.js"></script>

{{/* To render math when the KaTex parameter is set */}}
{{- if .Params.KaTex -}}
{{ partial "katex.html" . }}
{{- end -}}
```

- [ ] **Step 6: Commit**

```bash
cd /home/muad/cloud/website
git add layouts/partials
git commit -m "Add header/footer partials, rewire head.html to site.css, drop home modal"
```

(Do not build yet — config still points at the old theme; full verification happens in Task 6.)

---

### Task 4: Base template and homepage

**Files:**
- Create: `layouts/_default/baseof.html`
- Create: `layouts/index.html`

- [ ] **Step 1: Create `layouts/_default/baseof.html`**

The content wrapper must be `<main>` — backlinks.js and table-of-contents.js scan it.

```html
<!DOCTYPE html>
<html lang="{{ .Site.LanguageCode }}">
  <head>
    {{ partial "head.html" . }}
  </head>
  <body>
    {{ partial "header.html" . }}
    <main role="main">
      {{ block "main" . }}{{ end }}
    </main>
    {{ partial "footer.html" . }}
    {{ partial "extra_js.html" . }}
  </body>
</html>
```

- [ ] **Step 2: Create `layouts/index.html`** (homepage: hero + bio + publication cards)

```html
{{ define "main" }}
<section class="hero h-card">
  {{ with .Params.imgname }}
  <img class="hero-avatar u-photo" src="/{{ .name }}" alt="{{ .alt }}">
  {{ end }}
  <div>
    <h1 class="hero-name p-name">{{ .Params.name }}</h1>
    <p class="hero-title">{{ .Params.personal_title }}</p>
  </div>
</section>

<div class="hero-bio">
  {{ .Content }}
</div>

{{ with .Params.publications }}
<h2 class="section-heading" id="publications">Publications</h2>
<div class="publications">
  {{ range . }}
  <article class="pub-card" id="{{ anchorize .title }}"
           itemscope itemtype="http://schema.org/ScholarlyArticle">
    <div class="pub-thumb">
      <img src="/{{ .image }}" alt="Figure from: {{ .title }}" loading="lazy" itemprop="image">
    </div>
    <div class="pub-body">
      <h3 class="pub-title">
        <a href="{{ .pdf }}" itemprop="name">{{ .title }}</a>
      </h3>
      <p class="pub-meta">
        {{ range $i, $a := .authors }}{{ if $i }}, {{ end }}{{ if $a.me }}<strong class="pub-me" itemprop="author">{{ $a.name }}</strong>{{ else }}<span itemprop="author">{{ $a.name }}</span>{{ end }}{{ end }}
        {{ with .journal }} · <em itemprop="publisher">{{ . }}</em>{{ end }}
        · <span itemprop="datePublished">{{ .date }}</span>
      </p>
      {{ with .description }}
      <p class="pub-description" itemprop="description">{{ . }}</p>
      {{ end }}
      <div class="pub-links">
        <a href="{{ .pdf }}">PDF</a>
        {{ range .links }}<a href="{{ .url }}" itemprop="mainEntityOfPage">{{ .name }}</a>{{ end }}
        {{ with .citation }}
        <details class="pub-cite">
          <summary>Cite</summary>
          <pre><code>{{ . }}</code></pre>
        </details>
        {{ end }}
      </div>
    </div>
  </article>
  {{ end }}
</div>
{{ end }}
{{ end }}
```

- [ ] **Step 3: Commit**

```bash
cd /home/muad/cloud/website
git add layouts/_default/baseof.html layouts/index.html
git commit -m "Add base template and homepage with publication cards"
```

---

### Task 5: Section, taxonomy, and 404 templates

**Files:**
- Create: `layouts/partials/page-list.html`
- Create: `layouts/_default/list.html`
- Create: `layouts/_default/single.html`
- Create: `layouts/_default/terms.html`
- Create: `layouts/_default/taxonomy.html`
- Create: `layouts/404.html`

- [ ] **Step 1: Create `layouts/partials/page-list.html`** (shared by list + taxonomy pages)

Notes have no `date` (only `lastmod`), so fall back to `.Lastmod` when `.PublishDate` is zero.

```html
<ul class="post-list">
  {{ range .Pages.ByLastmod.Reverse }}
  <li class="post-item">
    <a class="post-link" href="{{ .Permalink }}">
      <span class="post-title">{{ .Title }}</span>
      {{ $d := cond .PublishDate.IsZero .Lastmod .PublishDate }}
      {{ if not $d.IsZero }}
      <time class="post-date" datetime="{{ $d.Format "2006-01-02" }}">{{ $d.Format "Jan 2, 2006" }}</time>
      {{ end }}
    </a>
    {{ $summary := or .Description (.Summary | plainify) }}
    {{ with $summary }}
    <p class="post-summary">{{ . | truncate 160 }}</p>
    {{ end }}
  </li>
  {{ end }}
</ul>
```

- [ ] **Step 2: Create `layouts/_default/list.html`**

```html
{{ define "main" }}
<header class="page-header">
  <h1>{{ .Title }}</h1>
</header>
{{ .Content }}
{{ partial "page-list.html" . }}
{{ end }}
```

- [ ] **Step 3: Create `layouts/_default/single.html`**

```html
{{ define "main" }}
<article class="article">
  <header>
    <h1 class="article-title">{{ .Title }}</h1>
    <div class="article-meta">
      {{ if not .Params.HideDate }}
        {{ if not .PublishDate.IsZero }}
        <time datetime="{{ .PublishDate.Format "2006-01-02" }}">{{ .PublishDate.Format "January 2, 2006" }}</time>
        {{ end }}
        {{ if .Params.lastmod }}
        · Updated <time datetime="{{ .Lastmod.Format "2006-01-02" }}">{{ .Lastmod.Format "January 2, 2006" }}</time>
        {{ end }}
      {{ end }}
    </div>
  </header>
  <div class="article-content">
    {{ .Content }}
  </div>
  <footer class="article-footer">
    {{ with .Params.tags }}
    <div class="term-list">
      {{ range . }}<a class="term-chip" href="/tags/{{ . | urlize }}/">#{{ . }}</a>{{ end }}
    </div>
    {{ end }}
    {{ with .Params.categories }}
    <div class="term-list">
      {{ range . }}<a class="term-chip" href="/categories/{{ . | urlize }}/">{{ . }}</a>{{ end }}
    </div>
    {{ end }}
  </footer>
</article>
{{ end }}
```

- [ ] **Step 4: Create `layouts/_default/terms.html`** (the /tags/ and /categories/ index pages)

```html
{{ define "main" }}
<header class="page-header">
  <h1>{{ .Title }}</h1>
</header>
<ul class="term-cloud">
  {{ range .Data.Terms.Alphabetical }}
  <li>
    <a class="term-chip" href="{{ .Page.Permalink }}">{{ .Page.Title }}<span class="term-count">{{ .Count }}</span></a>
  </li>
  {{ end }}
</ul>
{{ end }}
```

- [ ] **Step 5: Create `layouts/_default/taxonomy.html`** (a single tag/category page)

```html
{{ define "main" }}
<header class="page-header">
  <h1>{{ .Title }}</h1>
</header>
{{ partial "page-list.html" . }}
{{ end }}
```

- [ ] **Step 6: Create `layouts/404.html`**

```html
{{ define "main" }}
<div class="not-found">
  <h1>404</h1>
  <p>This page doesn't exist (or got moved).</p>
  <p><a href="{{ .Site.BaseURL }}">← Back to the homepage</a></p>
</div>
{{ end }}
```

- [ ] **Step 7: Commit**

```bash
cd /home/muad/cloud/website
git add layouts
git commit -m "Add list, single, taxonomy, terms, and 404 templates"
```

---

### Task 6: Switch config off the old theme and build

**Files:**
- Modify: `config.toml` (theme line ~6, menu entries lines ~30–48)

- [ ] **Step 1: Edit `config.toml`**

Change the theme line:

```toml
theme = ["osm","hugo-astatine-theme"]
```

to:

```toml
theme = ["osm"]
```

Replace the four `[[menu.main]]` blocks (Blog, Notes, Tags, Categories) with three (Tags/Categories leave the nav per the approved design; About is new):

```toml
[[menu.main]]
    name = "Blog"
    url = "/blog/"
    weight = -150

[[menu.main]]
    name = "Notes"
    url = "/notes/"
    weight = -140

[[menu.main]]
    name = "About"
    url = "/about/"
    weight = -130
```

- [ ] **Step 2: Clean build**

```bash
cd /home/muad/cloud/website
rm -rf public && hugo
```

Expected: build succeeds, no `ERROR` lines, summary table shows Pages built (≈40–60). Warnings about a missing `/about/` page are NOT expected (the menu link is just a URL); if Hugo errors about missing layouts, the lookup names in Tasks 4–5 need checking against Hugo 0.92 (`hugo version` to confirm).

- [ ] **Step 3: Spot-check generated output**

```bash
grep -c "pub-card" public/index.html
grep -o "css/site.css" public/index.html | head -1
grep -c "navbar-default" public/blog/index.html
grep -o "post-summary\|post-list" public/blog/index.html | sort -u
test -f public/tags/index.html && echo TAGS_OK
test -f public/404.html && echo 404_OK
grep -c "fonts.googleapis.com" public/index.html || echo NO_GOOGLE_FONTS
```

Expected: `4` pub-cards, `css/site.css` present, navbar present, `post-list` present, `TAGS_OK`, `404_OK`, and `NO_GOOGLE_FONTS` (count 0 → grep exits 1 → prints NO_GOOGLE_FONTS).

- [ ] **Step 4: Visual smoke test**

```bash
cd /home/muad/cloud/website && hugo server -p 1414
```

Open `http://localhost:1414` in a browser (or use Playwright MCP screenshots). Check: homepage hero + 4 publication cards (light), toggle button switches to dark and back, blog listing shows the hummus post with date, a note page renders, Cite expands BibTeX. Stop the server after.

- [ ] **Step 5: Commit (source only — public/ gets committed in Task 10)**

```bash
cd /home/muad/cloud/website
git add config.toml
git commit -m "Switch to custom layouts: drop hugo-astatine-theme from config"
```

---

### Task 7: About page and blog summaries

**Files:**
- Create: `content/about.md`
- Modify: `content/blog/20250112201223-hummus.md` (frontmatter)
- Modify: `content/blog/20240331083221-simulating_dipping_mdma.md` (frontmatter)

- [ ] **Step 1: Create `content/about.md`**

(The `page = "/:slug/"` permalink config does not apply to root-level pages in this setup; a root-level `about.md` renders at `/about/`, which is what the menu links to. Verify in step 3.)

```markdown
+++
title = "About"
+++

Hi, I'm Mua'ath — a neuroscientist studying how the brain processes
psychedelics and drives animal behavior.

My research spans thermosensation, interoception, and the neural basis of
internal states: how warmth is detected ([TRPV1 and TRPM2](https://doi.org/10.7554/eLife.95618.2)),
how sickness is represented in the insular cortex, and how internal states
can be read out from facial features across species.

Outside the lab I care about psychedelic harm reduction — you might find me
giving a talk at a rave. I also have strong opinions about hummus, which I
occasionally [write down](/blog/).

## Contact

- Email: muad.abdelhay [at] gmail [dot] com
- Publications: see the [homepage](/#publications)

<!-- TODO(muad): add CV link and any socials you want public -->
```

- [ ] **Step 2: Add summaries to blog post frontmatter**

In `content/blog/20250112201223-hummus.md`, add inside the `+++` block (after the `title` line):

```toml
description = "Strong opinions about hummus in Germany — and a recipe quest to finally make a proper one at home."
```

In `content/blog/20240331083221-simulating_dipping_mdma.md`, add inside the `+++` block (after the `title` line):

```toml
description = "Pharmacokinetic simulations of oral MDMA dosing strategies, benchmarked against published plasma-concentration data."
```

- [ ] **Step 3: Rebuild and verify**

```bash
cd /home/muad/cloud/website
hugo
test -f public/about/index.html && echo ABOUT_OK
grep -o "Strong opinions about hummus" public/blog/index.html | head -1
```

Expected: `ABOUT_OK` and the hummus summary string found.

- [ ] **Step 4: Commit**

```bash
cd /home/muad/cloud/website
git add content/about.md content/blog/20250112201223-hummus.md content/blog/20240331083221-simulating_dipping_mdma.md
git commit -m "Add About page and blog post summaries"
```

---

### Task 8: Re-point the injected JS styles at the new palette

**Files:**
- Modify: `static/js/backlinks.js`
- Modify: `static/js/notes-search.js`
- Modify: `static/js/table-of-contents.js`

The three sidebar scripts inject `<style>` blocks. Their `var(--bg-color, …)`-style references already resolve to the new tokens via the legacy aliases defined in site.css (Task 2). What remains are HARDCODED Tailwind-palette hexes (blues + cool grays) that clash with the warm palette.

- [ ] **Step 1: List the hardcoded colors**

```bash
cd /home/muad/cloud/website
grep -oh "#[0-9a-fA-F]\{6\}\b" static/js/backlinks.js static/js/notes-search.js static/js/table-of-contents.js | sort | uniq -c | sort -rn
```

- [ ] **Step 2: Replace them with token references**

Apply this mapping with sed (covers the Tailwind blues/grays these files use; the fallback value keeps each rule working even if site.css fails to load):

```bash
cd /home/muad/cloud/website
sed -i \
  -e 's/#3b82f6/var(--accent, #b08d57)/g' \
  -e 's/#2563eb/var(--accent, #b08d57)/g' \
  -e 's/#60a5fa/var(--accent, #c9a36a)/g' \
  -e 's/#93c5fd/var(--accent, #c9a36a)/g' \
  -e 's/#1f2937/var(--text, #1a1a1a)/g' \
  -e 's/#111827/var(--text, #1a1a1a)/g' \
  -e 's/#374151/var(--bg-card, #1f1c15)/g' \
  -e 's/#4b5563/var(--border, #2c2820)/g' \
  -e 's/#6b7280/var(--text-muted, #8a8378)/g' \
  -e 's/#9ca3af/var(--text-muted, #9a9284)/g' \
  -e 's/#d1d5db/var(--border, #ece7de)/g' \
  -e 's/#e5e7eb/var(--border, #ece7de)/g' \
  -e 's/#f3f4f6/var(--thumb-bg, #e8e2d8)/g' \
  -e 's/#f9fafb/var(--bg, #fdfbf7)/g' \
  -e 's/#f8fafc/var(--bg, #fdfbf7)/g' \
  -e 's/#e2e8f0/var(--border, #ece7de)/g' \
  -e 's/#ffffff/var(--bg-card, #ffffff)/g' \
  static/js/backlinks.js static/js/notes-search.js static/js/table-of-contents.js
```

Any hex from Step 1 NOT in this list: map it by role using the table below, editing the file directly. Pure white/near-white backgrounds → `--bg-card`; light gray borders → `--border`; mid grays (text) → `--text-muted`; dark grays (text) → `--text`; dark gray backgrounds (in `.dark` rules) → `--bg-card`; any blue → `--accent`.

Careful: do NOT replace hexes that appear OUTSIDE injected CSS strings (e.g. in canvas/SVG code) — check context with `grep -n -B2`.

- [ ] **Step 3: Check for double-wrapping accidents**

```bash
grep -n "var(--[a-z-]*, var(" static/js/*.js && echo "FIX NESTED VARS" || echo CLEAN
```

Expected: `CLEAN`. (Nesting can happen if a fallback inside an existing `var()` got substituted — e.g. `var(--bg-color, #ffffff)` became `var(--bg-color, var(--bg-card, #ffffff))`. That form is actually valid CSS, but if you prefer, restore those fallbacks to plain hexes.)

- [ ] **Step 4: Visual check of sidebars**

```bash
cd /home/muad/cloud/website && hugo server -p 1414
```

Open `http://localhost:1414/notes/20220405102340-psychedelics/` at ≥1400px width: TOC left + backlinks right should show warm card backgrounds and gold accents in both light and dark (toggle with the moon/sun button). Open `/notes/` and type "lsd" in the search box — results dropdown matches the palette. Stop the server.

- [ ] **Step 5: Commit**

```bash
cd /home/muad/cloud/website
git add static/js
git commit -m "Re-point injected sidebar styles at the new color tokens"
```

---

### Task 9: Cleanup, .gitignore, CLAUDE.md update

**Files:**
- Delete: `config.yml`, `config_ananke.toml`, `config_old.toml`, `content/_index_backup.md`, `content/blog/20250112201223-hummus.md~`, `content/muad@shulgin.13398:1694962527`, `static/css/centered-layout.css`, root screenshots, `.playwright-mcp/`, `public/_index_backup/`
- Create: `.gitignore`
- Modify: `CLAUDE.md`

- [ ] **Step 1: Delete cruft**

```bash
cd /home/muad/cloud/website
rm -f config.yml config_ananke.toml config_old.toml
rm -f content/_index_backup.md "content/blog/20250112201223-hummus.md~" "content/muad@shulgin.13398:1694962527"
rm -f blog-listing.png blog-post.png blog-post-dark.png homepage-dark.png homepage-full.png homepage-top.png
rm -rf .playwright-mcp public/_index_backup
rm -f static/css/centered-layout.css
git add -A content/ public/_index_backup 2>/dev/null; git status --short | head -30
```

Note: `content/#_index.md#` and `content/.#_index.md` already show as deleted in git status — `git add -A` in the commit step picks those up.

- [ ] **Step 2: Create `.gitignore`**

```gitignore
# Brainstorm/agent artifacts
.superpowers/

# Emacs
*~
\#*\#
.\#*

# Tooling artifacts
.playwright-mcp/
```

(`public/` stays committed — it is how the site is deployed.)

- [ ] **Step 3: Update `CLAUDE.md`** to match the new architecture. Replace the **Architecture → Configuration/Themes/Layout Overrides** sections and the **Responsive Layout** section with:

```markdown
### Configuration

- **Active config**: `config.toml` — theme list is `["osm"]` only
- **Base URL**: `http://cumol.org/`
- **Markup**: `goldmark.renderer.unsafe = true` — raw HTML allowed in markdown

### Themes & Layouts

- **All page templates are custom**, in `/layouts/` (baseof, index, list,
  single, terms, taxonomy, 404 + partials head/header/footer/extra_js/katex/page-list).
  There is no full theme dependency and no Tailwind/npm build step.
- **osm** (component, git submodule): provides only the
  `{{< openstreetmap mapName="..." >}}` shortcode.
- **Styling**: single hand-written stylesheet `/static/css/site.css`
  ("Refined Academic" design — warm paper light mode, warm dark mode, Lora +
  Inter self-hosted in `/static/fonts/`). All colors are CSS custom
  properties; dark mode = `.dark` class on `<html>` flipping tokens.
  Design spec: `docs/superpowers/specs/2026-06-10-website-redesign-design.md`.

## Responsive Layout

Content is a centered 720px column inside `<main>`. The JS-injected sidebars
(fixed-position) coexist via body classes:

- **≥1500px**: centered content, sidebars float free
- **1200–1499px**: `body.has-toc` adds left margin, `body.has-right-sidebar` adds right margin
- **<1200px**: site.css force-hides `.toc-sidebar`, `.backlinks-sidebar`, `.notes-search-container`
- **<768px**: hamburger nav (`toggleMenu()` toggles `.hidden` on `#navbar-default`); publication cards and hero stack vertically
```

Also delete the now-wrong line `**No .gitignore exists in this repo**` and the head.html/extra_js.html "Layout Overrides" description (they're no longer overrides — they're the only layouts).

- [ ] **Step 4: Verify the build still works after deletions**

```bash
cd /home/muad/cloud/website
rm -rf public && hugo
grep -rn "centered-layout" public/ layouts/ | grep -v Binary && echo "STALE REFERENCE" || echo CLEAN
```

Expected: build OK, `CLEAN`.

- [ ] **Step 5: Commit**

```bash
cd /home/muad/cloud/website
git add -A
git commit -m "Clean up legacy configs/backups, add .gitignore, update CLAUDE.md"
```

(This commit will include the regenerated `public/` — that's intended.)

---

### Task 10: Final verification pass

**Files:** none (verification + final commit of any straggler output)

- [ ] **Step 1: Clean rebuild**

```bash
cd /home/muad/cloud/website
rm -rf public && hugo
```

Expected: no errors.

- [ ] **Step 2: Page inventory check**

```bash
cd /home/muad/cloud/website
for p in index.html about/index.html blog/index.html notes/index.html tags/index.html categories/index.html 404.html blog/20250112201223-hummus/index.html notes/20220405102340-psychedelics/index.html; do
  test -f "public/$p" && echo "OK  $p" || echo "MISSING  $p"
done
grep -L "site.css" public/index.html public/blog/index.html public/about/index.html || echo "ALL_PAGES_HAVE_SITE_CSS"
```

Expected: all `OK`, then `ALL_PAGES_HAVE_SITE_CSS`.

- [ ] **Step 3: Full visual walkthrough** (browser or Playwright MCP, viewport ≥1400px and ~390px mobile)

1. Homepage light: hero avatar, name, gold "NEUROSCIENTIST" label, bio, 4 publication cards with thumbnails; bold "Abd El Hay, M. Y." in author lists.
2. Click moon → dark mode: warm near-black background, cards visible, gold accents; reload page → dark persists with no flash of light theme.
3. Cite on first publication → BibTeX block expands, full width, readable.
4. `/blog/` → hummus post with date + summary line. Open it: title, date, content, `#hummus` `#recipes` chips; TOC sidebar on the left (the post has 2+ headings).
5. `/notes/` → 6 notes listed; search "lsd" → dropdown results styled to match.
6. Open the LSD note: backlinks sidebar right, warm styling.
7. `/about/`, `/tags/`, a tag page like `/tags/hummus/`, and a bogus URL for the 404 — all render in the new design.
8. Mobile width: hamburger opens the menu; publication cards stacked; no horizontal scroll.
9. The hummus post mentions a uMap — verify any `{{</* openstreetmap */>}}` embed still renders (osm theme intact).

- [ ] **Step 4: Commit any remaining regenerated output**

```bash
cd /home/muad/cloud/website
git add -A
git status --short | head -5
git commit -m "Rebuild site with redesigned templates" || echo "nothing left to commit"
```

---

## Self-review notes (already applied)

- **Spec coverage:** fonts (T1), CSS/tokens/dark (T2), head/header/footer/KaTeX (T3), homepage + publication format unchanged (T4), list/single/taxonomy/terms/404 + blog summaries display (T5, T7), config/theme switch + nav change (T6), About page (T7), JS restyle (T8), cleanup + .gitignore + CLAUDE.md (T9), verification incl. responsive + no-flash + osm + KaTeX hooks (T6/T10).
- **Hugo 0.92 compatibility:** templates only use functions available in 0.92 (`cond`, `anchorize`, `urlize`, `plainify`, `truncate`, `.Data.Terms.Alphabetical` with `.Page`). Do not introduce `hugo.IsServer`, `page.Store`, etc.
- **Known judgment point (Task 8):** hex list in the sed command may not cover every hardcoded color in notes-search.js / table-of-contents.js — Step 1 enumerates them and the role-mapping table covers the remainder. Verify visually in Step 4.
```

<script lang="ts">
  import { themeState } from '../../themeState.svelte';
  import FlutteringRing from './FlutteringRing.svelte';

  let { provData } = $props<{ provData: any }>();

  let isMetaOpen = $state(false);

  let environment = $derived(provData?.entity?.['rdt:environment'] || {});
  let scriptName = $derived(environment?.['rdt:script'] ? environment['rdt:script'].split(/[/\\]/).pop() : 'script.R');
  let rawTime = $derived(environment?.['rdt:provTimestamp'] || '');
  let executionTime = $derived(
    rawTime
      ? rawTime.replace('T', ' ').replace(/([A-Z]{3,4})$/, '').trim()
      : '14:22:05'
  );
  let architecture = $derived(environment?.['rdt:architecture'] || 'x86_64');
  let os = $derived(environment?.['rdt:operatingSystem'] || 'macOS');
  let rawRVersion = $derived(environment?.['rdt:langVersion'] || 'R 4.3.2');
  let rVersion = $derived(rawRVersion.replace(/^R version\s*/i, 'R '));
  let totalTime = $derived(environment?.['rdt:totalElapsedTime'] || '0.42');

  // Count activities and entities
  const activities = $derived(provData?.activity || {});
  const entities = $derived(provData?.entity || {});
  const actCount = $derived(Object.keys(activities).length || 6);
  const entCount = $derived(Object.keys(entities).filter(k => !k.includes('environment')).length || 5);

  const styleMode = $derived(provData?.coverStyle || themeState.coverStyle || 'modern');
</script>

{#if styleMode === 'modern'}
  <!-- Modern Architectural Cover Layout (surface-cover-modern) -->
  <div class="cover cover-modern">
    <!-- Top Header Bar -->
    <div class="cover-top-bar">
      <span class="provbook-pill">provbook</span>
      <span class="cover-subtitle">An interactive record of code execution and data lineage</span>
    </div>

    <!-- Dead Center Script Title with Generative Fluttering Ring & FAB -->
    <div class="cover-hero">
      <div class="ring-canvas-layer">
        <FlutteringRing />
      </div>
      <div class="hero-center-content">
        <h1 class="script-title">{scriptName}</h1>
        <!-- FAB (+) Toggle Button -->
        <button 
          class="fab-toggle-btn"
          class:open={isMetaOpen}
          onclick={(e) => { e.stopPropagation(); isMetaOpen = !isMetaOpen; }}
          aria-label="Toggle Metadata Overview"
          title="Toggle metadata overview"
        >
          <svg xmlns="http://www.w3.org/2000/svg" width="10" height="10" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="3" stroke-linecap="round" stroke-linejoin="round">
            {#if isMetaOpen}
              <line x1="18" y1="6" x2="6" y2="18"></line>
              <line x1="6" y1="6" x2="18" y2="18"></line>
            {:else}
              <line x1="12" y1="5" x2="12" y2="19"></line>
              <line x1="5" y1="12" x2="19" y2="12"></line>
            {/if}
          </svg>
        </button>
      </div>
    </div>

    <!-- Architectural Footer Lockup (Popover Card triggered by FAB) -->
    <div class="cover-footer">
      {#if isMetaOpen}
        <!-- svelte-ignore a11y_click_events_have_key_events -->
        <!-- svelte-ignore a11y_no_static_element_interactions -->
        <div class="footer-left-lockup popover-card" onclick={(e) => e.stopPropagation()}>
          <!-- Left Metadata Column -->
          <div class="meta-col">
            <div class="meta-item"><span class="m-key">Language</span><span class="m-val">{rVersion}</span></div>
            <div class="meta-item"><span class="m-key">System</span><span class="m-val">{os}</span></div>
            <div class="meta-item"><span class="m-key">Recorded</span><span class="m-val">{executionTime}</span></div>
            <div class="meta-item"><span class="m-key">Duration</span><span class="m-val">{totalTime}s</span></div>
          </div>

          <!-- Vertical Hairline Divider -->
          <div class="footer-divider"></div>

          <!-- Stacked Stats Column -->
          <div class="stats-col">
            <div class="stat-unit">
              <span class="stat-num">{actCount}</span>
              <span class="stat-lbl">Operations</span>
            </div>
            <div class="stat-unit">
              <span class="stat-num">{entCount}</span>
              <span class="stat-lbl">Data Objects</span>
            </div>
          </div>
        </div>
      {/if}

      <!-- Right Action Pill -->
      <div class="action-col">
        <span class="begin-pill">→ to begin</span>
      </div>
    </div>
  </div>
{:else}
  <!-- Classic Centered Cover Layout -->
  <div class="cover cover-classic">
    <p class="chapter-label">provbook</p>
    <h1>{scriptName}</h1>
    <p class="subtitle">An interactive record of code execution,<br>data lineage, and computational history.</p>

    <div class="meta-table">
      <div class="meta-row">
        <span class="meta-key">Language</span>
        <span class="meta-val">{rVersion}</span>
      </div>
      <div class="meta-row">
        <span class="meta-key">System</span>
        <span class="meta-val">{os} ({architecture})</span>
      </div>
      <div class="meta-row">
        <span class="meta-key">Recorded</span>
        <span class="meta-val">{executionTime}</span>
      </div>
      <div class="meta-row">
        <span class="meta-key">Duration</span>
        <span class="meta-val">{totalTime}s</span>
      </div>
    </div>

    <div class="stats">
      <div class="stat">
        <span class="stat-num">{actCount}</span>
        <span class="stat-label">Operations</span>
      </div>
      <div class="stat">
        <span class="stat-num">{entCount}</span>
        <span class="stat-label">Data Objects</span>
      </div>
    </div>

    <p class="hint">Click the right page or press → to begin</p>
  </div>
{/if}

<style>
  /* ── Common Base ── */
  .cover {
    width: 100%;
    height: 100%;
    min-height: 100%;
    box-sizing: border-box;
    color: var(--text);
  }

  /* ── Modern Architectural Cover (surface-cover-modern) ── */
  .cover-modern {
    display: flex;
    flex-direction: column;
    justify-content: space-between;
    padding: 1.35rem 1.5rem 1.35rem;
  }

  .cover-top-bar {
    display: flex;
    justify-content: space-between;
    align-items: center;
    width: 100%;
  }

  .provbook-pill {
    font-family: var(--font-sans);
    font-size: 0.74rem;
    font-weight: 600;
    color: var(--text);
    padding: 0.22rem 0.72rem;
    background: var(--pill-bg, rgba(0, 0, 0, 0.07));
    border-radius: 9999px;
    letter-spacing: -0.01em;
  }

  :global(.mode-dark) .provbook-pill,
  .mode-dark .provbook-pill {
    background: rgba(255, 255, 255, 0.12);
  }

  .cover-subtitle {
    font-family: var(--font-sans);
    font-size: 0.7rem;
    color: var(--text-secondary);
    letter-spacing: -0.01em;
  }

  .cover-hero {
    position: relative;
    display: flex;
    align-items: center;
    justify-content: center;
    flex: 1;
    text-align: center;
    padding: 2rem 0;
    overflow: hidden;
  }

  .ring-canvas-layer {
    position: absolute;
    inset: 0;
    display: flex;
    align-items: center;
    justify-content: center;
    pointer-events: none;
    z-index: 0;
    opacity: 0.85;
  }

  .hero-center-content {
    position: relative;
    z-index: 1;
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 0.8rem;
  }

  .cover-hero .script-title {
    position: relative;
    z-index: 1;
    font-family: var(--font-sans);
    font-size: 1.7rem;
    font-weight: 500;
    color: var(--text);
    margin: 0;
    letter-spacing: -0.02em;
  }

  .fab-toggle-btn {
    position: relative;
    z-index: 2;
    width: 22px;
    height: 22px;
    border-radius: 50%;
    background: var(--pill-bg, rgba(0, 0, 0, 0.05));
    border: 1px solid var(--border-subtle, rgba(0, 0, 0, 0.12));
    display: inline-flex;
    align-items: center;
    justify-content: center;
    color: var(--text-secondary);
    cursor: pointer;
    transition: all 0.2s ease;
    padding: 0;
  }

  .fab-toggle-btn:hover {
    transform: scale(1.12);
    background: var(--pill-bg-hover, rgba(0, 0, 0, 0.1));
    color: var(--text);
  }

  .fab-toggle-btn.open {
    background: var(--text);
    color: var(--bg-warm, #faf9f7);
    border-color: var(--text);
  }

  .cover-footer {
    position: relative;
    display: flex;
    justify-content: flex-end;
    align-items: center;
    width: 100%;
    min-height: 32px;
  }

  .footer-left-lockup {
    position: absolute;
    bottom: 2.8rem;
    left: 50%;
    transform: translateX(-50%);
    display: flex;
    align-items: center;
    gap: 1.5rem;
    padding: 0.75rem 1.4rem;
    background: var(--popover-bg, var(--bg-warm, #f7f6f1));
    border: 1px solid var(--popover-border, var(--border, rgba(0, 0, 0, 0.12)));
    border-radius: 14px;
    backdrop-filter: blur(16px);
    -webkit-backdrop-filter: blur(16px);
    box-shadow: 0 4px 20px rgba(0, 0, 0, 0.05);
    z-index: 10;
    pointer-events: auto;
    max-width: calc(100% - 2rem);
    box-sizing: border-box;
    width: max-content;
  }

  :global(.theme-eink-dark) .footer-left-lockup,
  :global(.mode-dark) .footer-left-lockup,
  .mode-dark .footer-left-lockup {
    background: var(--popover-bg, #1e1e24);
    border-color: var(--popover-border, rgba(255, 255, 255, 0.15));
  }

  .meta-col {
    display: flex;
    flex-direction: column;
    gap: 0.35rem;
  }

  .meta-item {
    display: flex;
    align-items: center;
    gap: 1.2rem;
    font-family: var(--font-sans);
    font-size: 0.64rem;
    line-height: 1.25;
    white-space: nowrap;
    font-variant-numeric: tabular-nums;
  }

  .m-key {
    color: var(--text-muted);
    font-size: 0.62rem;
    min-width: 68px;
    letter-spacing: -0.01em;
  }

  .m-val {
    color: var(--text);
    font-weight: 500;
    font-size: 0.64rem;
    white-space: nowrap;
  }

  .footer-divider {
    width: 1px;
    height: 52px;
    background: var(--border, rgba(0, 0, 0, 0.1));
    flex-shrink: 0;
  }

  :global(.mode-dark) .footer-divider,
  .mode-dark .footer-divider {
    background: rgba(255, 255, 255, 0.15);
  }

  .stats-col {
    display: flex;
    flex-direction: column;
    gap: 0.45rem;
    white-space: nowrap;
  }

  .stat-unit {
    display: flex;
    align-items: baseline;
    gap: 0.45rem;
    line-height: 1.1;
    white-space: nowrap;
  }

  .stat-unit .stat-num {
    font-family: var(--font-sans);
    font-size: 1.2rem;
    font-weight: 700;
    color: var(--text);
  }

  .stat-unit .stat-lbl {
    font-family: var(--font-sans);
    font-size: 0.64rem;
    color: var(--text-muted);
    letter-spacing: -0.01em;
    white-space: nowrap;
  }

  @media (max-width: 580px) {
    .footer-left-lockup {
      gap: 1rem;
      padding: 0.6rem 1rem;
    }
    .m-key {
      min-width: 58px;
    }
  }

  .action-col {
    display: flex;
    align-items: center;
    pointer-events: auto;
  }

  .begin-pill {
    display: inline-flex;
    align-items: center;
    font-family: var(--font-sans);
    font-size: 0.7rem;
    font-weight: 500;
    color: var(--text-secondary);
    padding: 0.3rem 0.85rem;
    background: var(--pill-bg, rgba(0, 0, 0, 0.06));
    border-radius: 9999px;
    letter-spacing: -0.01em;
  }

  :global(.mode-dark) .begin-pill,
  .mode-dark .begin-pill {
    background: rgba(255, 255, 255, 0.1);
  }

  /* ── Classic Cover Layout ── */
  .cover-classic {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    text-align: center;
    padding: 3rem 2rem;
  }
  .cover-classic h1 {
    font-size: 2.2rem;
    margin-bottom: 0.6rem;
    letter-spacing: -0.03em;
    color: var(--text);
  }
  .subtitle {
    color: var(--text-secondary);
  }
  .meta-table {
    width: 100%;
    max-width: 320px;
    border-top: 1px solid var(--border);
    border-bottom: 1px solid var(--border);
    padding: 1.2rem 0;
    margin: 2rem 0;
  }
  .meta-row {
    display: flex;
    justify-content: space-between;
    padding: 0.3rem 0;
    font-family: var(--font-mono, monospace);
    font-size: 0.78rem;
  }
  .meta-key { color: var(--text-muted); }
  .meta-val { color: var(--text); font-weight: 600; }
  .stats {
    display: flex;
    gap: 3rem;
    margin: 1rem 0 2rem;
  }
  .stat {
    display: flex;
    flex-direction: column;
    align-items: center;
  }
  .stat-num {
    font-size: 2rem;
    font-weight: 700;
    color: var(--accent);
    line-height: 1;
  }
  .stat-label {
    font-family: var(--font-sans);
    font-size: 0.72rem;
    text-transform: uppercase;
    letter-spacing: 0.1em;
    color: var(--text-muted);
    margin-top: 0.3rem;
  }
  .hint {
    font-family: var(--font-sans);
    font-size: 0.75rem;
    color: var(--text-muted);
    margin-top: 1rem;
  }
</style>

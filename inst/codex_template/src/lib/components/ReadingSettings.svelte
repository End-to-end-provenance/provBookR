<script lang="ts">
  import type { SweepStyle } from './pageCurlTypes';

  interface Props {
    fontSize: number;
    fontFamily: string;
    lineHeight: number;
    justified: boolean;
    curlEnabled: boolean;
    curlRadius: number;
    curlSpeed: number;
    curlMode: 'curl' | 'ripple';
    curlSweep: SweepStyle;
  }

  let {
    fontSize = $bindable(14),
    fontFamily = $bindable('sans'),
    lineHeight = $bindable(1.78),
    justified = $bindable(true),
    curlEnabled = $bindable(true),
    curlRadius = $bindable(0.08),
    curlSpeed = $bindable(0.7),
    curlMode = $bindable<'curl' | 'ripple'>('curl'),
    curlSweep = $bindable<SweepStyle>('corner'),
  }: Props = $props();

  let open = $state(false);

  function toggle(e: MouseEvent) {
    e.stopPropagation();
    open = !open;
  }

  const fontFamilies = [
    { label: 'Serif', value: 'serif' },
    { label: 'Sans', value: 'sans' },
  ];
  const fontSizes = [12, 13, 14, 15, 16, 17, 18, 20, 22];
  const lineHeights = [
    { label: 'Tight', value: 1.5 },
    { label: 'Normal', value: 1.78 },
    { label: 'Loose', value: 2.1 },
  ];
  const curlRadii = [
    { label: 'Tight', value: 0.04 },
    { label: 'Normal', value: 0.08 },
    { label: 'Wide', value: 0.14 },
  ];
  const curlSpeeds = [
    { label: 'Fast', value: 0.4 },
    { label: 'Normal', value: 0.7 },
    { label: 'Slow', value: 1.2 },
  ];
  const sweepDirections: { label: string; value: SweepStyle }[] = [
    { label: 'Horizontal', value: 'horizontal' },
    { label: 'Corner', value: 'corner' },
  ];
</script>

<svelte:window onclick={() => { if (open) open = false; }} />

<div class="settings-wrap">
  <button
    class="settings-btn"
    class:active={open}
    onclick={toggle}
    title="Reading settings"
    aria-label="Reading settings"
  >Aa</button>

  {#if open}
    <!-- svelte-ignore a11y_click_events_have_key_events -->
    <!-- svelte-ignore a11y_no_static_element_interactions -->
    <div class="panel" onclick={(e: MouseEvent) => e.stopPropagation()}>
      <div class="panel-title">Reading</div>

      <div class="row">
        <span class="label">Font</span>
        <div class="group">
          {#each fontFamilies as f}
            <button
              class="opt" class:sel={fontFamily === f.value}
              onclick={() => { fontFamily = f.value; }}
              style={f.value === 'serif' ? 'font-family:Georgia,serif' : 'font-family:system-ui,sans-serif'}
            >{f.label}</button>
          {/each}
        </div>
      </div>

      <div class="row">
        <span class="label">Size</span>
        <div class="group">
          {#each fontSizes as s}
            <button class="opt sz" class:sel={fontSize === s} onclick={() => { fontSize = s; }}>{s}</button>
          {/each}
        </div>
      </div>

      <div class="row">
        <span class="label">Spacing</span>
        <div class="group">
          {#each lineHeights as lh}
            <button class="opt" class:sel={lineHeight === lh.value} onclick={() => { lineHeight = lh.value; }}>{lh.label}</button>
          {/each}
        </div>
      </div>

      <div class="row">
        <span class="label">Justify</span>
        <button class="opt tog" class:sel={justified} onclick={() => { justified = !justified; }}>{justified ? 'On' : 'Off'}</button>
      </div>

    </div>
  {/if}
</div>

<style>
  .settings-wrap { position: relative; }

  .settings-btn {
    font-family: var(--font-sans);
    font-size: 0.75rem;
    padding: 0.3rem 0.6rem;
    border: 1px solid var(--border, #e5e7eb);
    border-radius: 4px;
    background: #fff;
    color: var(--text-muted, #94a3b8);
    cursor: pointer;
    transition: color 0.15s, background 0.15s, border-color 0.15s;
  }
  .settings-btn:hover, .settings-btn.active {
    color: var(--accent, #2563eb);
    border-color: var(--accent, #2563eb);
    background: var(--accent-light, #dbeafe);
  }

  .panel {
    position: absolute;
    bottom: calc(100% + 8px);
    right: 0;
    background: #fff;
    border: 1px solid var(--border, #e5e7eb);
    border-radius: 8px;
    padding: 1rem 1.1rem;
    box-shadow: 0 4px 20px rgba(0,0,0,0.1);
    z-index: 200;
    min-width: 300px;
  }

  .panel-title {
    font-family: var(--font-sans);
    font-size: 0.65rem;
    text-transform: uppercase;
    letter-spacing: 0.1em;
    color: var(--text-muted, #94a3b8);
    margin-bottom: 0.75rem;
  }

  .divider {
    height: 1px;
    background: var(--border, #e5e7eb);
    margin: 0.75rem 0;
  }

  .row { display: flex; align-items: center; gap: 0.75rem; margin-bottom: 0.6rem; }
  .row:last-child { margin-bottom: 0; }

  .label {
    font-family: var(--font-sans);
    font-size: 0.75rem;
    color: var(--text-secondary, #555);
    width: 3.5rem;
    flex-shrink: 0;
  }

  .group { display: flex; gap: 0.3rem; flex-wrap: wrap; }

  .opt {
    font-family: var(--font-sans);
    font-size: 0.72rem;
    padding: 0.25rem 0.55rem;
    border: 1px solid var(--border, #e5e7eb);
    border-radius: 4px;
    background: #fff;
    color: var(--text-secondary, #555);
    cursor: pointer;
    transition: background 0.12s, border-color 0.12s, color 0.12s;
    white-space: nowrap;
  }
  .opt:hover { background: var(--bg-figure, #fafbfc); }
  .opt.sel { background: var(--accent, #2563eb); border-color: var(--accent, #2563eb); color: #fff; }
  .sz { min-width: 2rem; text-align: center; }
  .tog { min-width: 2.5rem; text-align: center; }
</style>

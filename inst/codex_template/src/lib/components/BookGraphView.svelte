<script lang="ts">
  import { bookNavigationState, viewModeState } from '../state.svelte';
  import Dock from './Dock.svelte';

  interface Props {
    spreads: { left?: any; right?: any; cover?: any; label: string }[];
    provData: any;
  }

  let { spreads = [], provData = {} }: Props = $props();

  function selectSpread(index: number) {
    bookNavigationState.currentSpread = index;
    viewModeState.mode = 'book';
  }
</script>

<div class="slide-sorter book-graph-view">
  <div class="sorter-header">
    <h2>Book Overview</h2>
    <p>Click any spread to jump directly into that chapter.</p>
  </div>

  <div class="sorter-grid">
    {#each spreads as spread, i}
      <!-- svelte-ignore a11y_click_events_have_key_events -->
      <!-- svelte-ignore a11y_no_static_element_interactions -->
      <div 
        class="slide-card" 
        class:active={bookNavigationState.currentSpread === i}
        onclick={() => selectSpread(i)}
      >
        <div class="slide-frame">
          {#if spread.cover}
            <!-- Cover: single page -->
            <div class="slide-content cover-content-frame">
              <div class="scale-wrapper">
                {#if spread.cover}
                  {@const Cover = spread.cover}
                  <Cover {provData} />
                {/if}
              </div>
            </div>
          {:else}
            <!-- Two-page spread -->
            <div class="slide-content spread-content-frame">
              <div class="spread-half left-half">
                <div class="scale-wrapper">
                  {#if spread.left}
                    {@const Left = spread.left}
                    <Left {provData} />
                  {/if}
                </div>
              </div>
              <div class="spread-spine"></div>
              <div class="spread-half right-half">
                <div class="scale-wrapper">
                  {#if spread.right}
                    {@const Right = spread.right}
                    <Right {provData} />
                  {/if}
                </div>
              </div>
            </div>
          {/if}
        </div>
        <span class="slide-label">{spread.label}</span>
        <span class="slide-number">{i + 1}</span>
      </div>
    {/each}
  </div>
</div>

<Dock />

<style>
  .slide-sorter {
    width: 100vw;
    min-height: 100vh;
    padding: 5rem 3rem 8rem;
    box-sizing: border-box;
    background: var(--bg-warm, #faf9f7);
    overflow-y: auto;
  }

  .sorter-header {
    text-align: center;
    margin-bottom: 3rem;
  }
  .sorter-header h2 {
    font-family: var(--font-sans);
    font-size: 1.8rem;
    font-weight: 700;
    margin: 0 0 0.5rem;
    color: var(--text, #1a1a24);
    letter-spacing: -0.02em;
  }
  .sorter-header p {
    font-family: var(--font-sans);
    color: var(--text-secondary, #4a4a5a);
    font-size: 0.9rem;
    margin: 0;
  }

  .sorter-grid {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(320px, 1fr));
    gap: 2rem;
    max-width: 1400px;
    margin: 0 auto;
  }

  .slide-card {
    position: relative;
    cursor: pointer;
    transition: transform 0.25s cubic-bezier(0.16, 1, 0.3, 1), box-shadow 0.25s;
  }
  .slide-card:hover {
    transform: translateY(-4px);
  }

  .slide-frame {
    border-radius: 12px;
    overflow: hidden;
    background: var(--page-bg, #ffffff);
    border: 2px solid var(--border, #e5e7eb);
    box-shadow: 0 4px 16px -4px rgba(0, 0, 0, 0.08);
    transition: border-color 0.2s, box-shadow 0.2s;
    aspect-ratio: 16 / 10;
  }
  .slide-card:hover .slide-frame {
    border-color: var(--card-border-active, rgba(0, 0, 0, 0.2));
    box-shadow: 0 12px 32px -8px rgba(0, 0, 0, 0.12);
  }
  .slide-card.active .slide-frame {
    border-color: var(--text, #1a1a24);
    box-shadow: 0 0 0 3px var(--accent, rgba(26, 26, 36, 0.15)), 0 12px 32px -8px rgba(0, 0, 0, 0.15);
  }

  .slide-content {
    width: 100%;
    height: 100%;
    position: relative;
    overflow: hidden;
  }

  /* Single cover page */
  .cover-content-frame {
    display: flex;
    align-items: center;
    justify-content: center;
  }
  .cover-content-frame .scale-wrapper {
    width: 800px;
    height: 500px;
    transform: scale(0.4);
    transform-origin: top left;
    position: absolute;
    top: 0;
    left: 50%;
    margin-left: -160px; /* half of 800 * 0.4 = 160 */
    pointer-events: none;
    overflow: hidden;
  }

  /* Two-page spread */
  .spread-content-frame {
    display: flex;
    height: 100%;
  }

  .spread-half {
    flex: 1;
    position: relative;
    overflow: hidden;
  }

  .spread-half .scale-wrapper {
    width: 640px;
    height: 500px;
    transform: scale(0.25);
    transform-origin: top left;
    position: absolute;
    top: 0;
    left: 0;
    pointer-events: none;
    padding: 2rem;
    box-sizing: border-box;
  }

  .right-half .scale-wrapper {
    left: auto;
    right: 0;
    transform-origin: top right;
  }

  .spread-spine {
    width: 1px;
    background: linear-gradient(
      to bottom,
      transparent 5%,
      var(--border, #e5e7eb) 15%,
      var(--border, #e5e7eb) 85%,
      transparent 95%
    );
    flex-shrink: 0;
  }

  .left-half {
    border-right: none;
  }

  .slide-label {
    display: block;
    text-align: center;
    margin-top: 0.75rem;
    font-family: var(--font-sans);
    font-size: 0.8rem;
    font-weight: 600;
    color: var(--text, #1a1a24);
  }

  .slide-number {
    position: absolute;
    top: 0.5rem;
    left: 0.5rem;
    width: 22px;
    height: 22px;
    border-radius: 50%;
    background: var(--glass-bg, rgba(255, 255, 255, 0.85));
    backdrop-filter: blur(8px);
    border: 1px solid var(--glass-border, rgba(255, 255, 255, 0.6));
    display: flex;
    align-items: center;
    justify-content: center;
    font-family: var(--font-mono, monospace);
    font-size: 0.6rem;
    font-weight: 700;
    color: var(--text-secondary, #4a4a5a);
    z-index: 2;
  }
  .slide-card.active .slide-number {
    background: var(--pill-badge-bg, var(--text));
    color: var(--pill-badge-text, #ffffff);
    border-color: var(--border, var(--text));
  }
</style>

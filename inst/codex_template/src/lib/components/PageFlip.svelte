<script lang="ts">
  import { onMount } from 'svelte';
  import provData from '$lib/data/prov.json';
  import ReadingSettings from './ReadingSettings.svelte';
  import PageCurl from './PageCurl.svelte';
  import Dock from './Dock.svelte';
  import Icon from './Icon.svelte';
  import { themeState } from '../themeState.svelte';
  import { activeScenarioState } from '../state.svelte';
  import { resolveSweep } from './pageCurlTypes';
  import type { SweepStyle } from './pageCurlTypes';

  interface Props {
    currentSpread: number;
    totalSpreads: number;
    onNext: () => void;
    onPrev: () => void;
    isCover?: boolean;
    coverPage?: import('svelte').Snippet;
    leftPage: import('svelte').Snippet;
    rightPage: import('svelte').Snippet;
    mobilePages?: any[];
  }

  let {
    currentSpread, totalSpreads, onNext, onPrev,
    isCover = false, coverPage, leftPage, rightPage,
    mobilePages = [],
  }: Props = $props();

  let flipping = $state(false);
  let flipDirection = $state<'next' | 'prev'>('next');
  let showCurl = $state(false);
  let curlRef = $state<PageCurl>();

  // Element refs for scroll reset
  let leftHalf = $state<HTMLDivElement>();
  let rightHalf = $state<HTMLDivElement>();

  // Scroll overflow indicators
  let leftCanScroll = $state(false);
  let rightCanScroll = $state(false);

  function checkScrollState() {
    if (leftHalf) {
      leftCanScroll = leftHalf.scrollHeight - leftHalf.scrollTop - leftHalf.clientHeight > 20;
    }
    if (rightHalf) {
      rightCanScroll = rightHalf.scrollHeight - rightHalf.scrollTop - rightHalf.clientHeight > 20;
    }
  }

  function scrollLeftDown(e: MouseEvent) {
    e.stopPropagation();
    leftHalf?.scrollBy({ top: 320, behavior: 'smooth' });
  }

  function scrollRightDown(e: MouseEvent) {
    e.stopPropagation();
    rightHalf?.scrollBy({ top: 320, behavior: 'smooth' });
  }

  // Reading settings mapped to global themeState
  const fontSize = $derived(themeState.fontSize);
  const fontFamily = $derived(themeState.fontFamily);
  const lineHeight = $derived(themeState.lineHeight);
  const justified = $derived(themeState.justified);

  // Curl settings mapped to global themeState
  const curlEnabled = $derived(themeState.curlEnabled);
  const curlRadius = $derived(themeState.curlRadius);
  const curlSpeed = $derived(themeState.curlSpeed);
  const curlMode = $derived(themeState.curlMode);
  const curlSweep = $derived(themeState.curlSweep);

  // ── Mobile state ──
  let isMobile = $state(false);
  let mobileAccepted = $state(false);
  let mobileCard = $state(0);
  let slideDir = $state<'next' | 'prev' | null>(null);
  let animating = $state(false);

  // Touch tracking for mobile swipe
  let mTouchStartX = 0;
  let mTouchStartY = 0;
  let mSwiping = $state(false);
  let mSwipeX = $state(0);

  const totalMobileCards = $derived(mobilePages.length);

  $effect(() => {
    const r = document.documentElement;
    r.style.setProperty('--reading-size', fontSize + 'px');
    r.style.setProperty('--reading-lh', String(lineHeight));
    r.style.setProperty('--reading-font',
      fontFamily === 'serif'
        ? '"Palatino Linotype","Book Antiqua",Palatino,Georgia,serif'
        : '-apple-system,BlinkMacSystemFont,"Segoe UI",Helvetica,Arial,sans-serif'
    );
  });

  // Reset scroll to top when navigating to a new spread
  $effect(() => {
    // Touch currentSpread to subscribe
    void currentSpread;
    leftHalf?.scrollTo(0, 0);
    rightHalf?.scrollTo(0, 0);
    setTimeout(checkScrollState, 150);
  });

  // Reset scroll to top when navigating mobile cards
  $effect(() => {
    void mobileCard;
    document.querySelector('.mobile-card.card-active')?.scrollTo(0, 0);
  });

  // ── Desktop touch / flip logic ──
  let touchStartX = 0;

  function flipWithCurl(dir: 'next' | 'prev', advanceFn: () => void) {
    if (flipping) return;
    flipping = true;

    if (!curlEnabled) {
      advanceFn();
      flipping = false;
      return;
    }

    flipDirection = dir;
    showCurl = true;

    requestAnimationFrame(() => {
      requestAnimationFrame(() => {
        if (!curlRef) {
          advanceFn();
          showCurl = false;
          flipping = false;
          return;
        }

        const swapFraction = curlMode === 'ripple' ? 0.75 : 0.5;
        const swapDelay = curlSpeed * 1000 * swapFraction;
        let swapped = false;
        const swapTimer = setTimeout(() => {
          swapped = true;
          advanceFn();
        }, swapDelay);

        curlRef.play().then(() => {
          if (!swapped) {
            clearTimeout(swapTimer);
            advanceFn();
          }
          showCurl = false;
          flipping = false;
        });
      });
    });
  }

  function flipNext() {
    if (flipping || currentSpread >= totalSpreads - 1) return;
    if (isCover) {
      onNext();
      return;
    }
    flipWithCurl('next', onNext);
  }

  function flipPrev() {
    if (flipping || currentSpread <= 0) return;
    if (currentSpread === 1) {
      onPrev();
      return;
    }
    flipWithCurl('prev', onPrev);
  }

  function handleKey(e: KeyboardEvent) {
    const target = e.target as HTMLElement | null;
    const isInput = target && (
      target.tagName === 'INPUT' || 
      target.tagName === 'TEXTAREA' || 
      target.isContentEditable ||
      Boolean(target.closest('input, textarea, [contenteditable]'))
    );
    if (isInput) return; // Do not intercept typing or spacebar in input fields

    if (isMobile) {
      if (e.key === 'ArrowRight' || e.key === ' ') { e.preventDefault(); mobileNext(); }
      else if (e.key === 'ArrowLeft') { e.preventDefault(); mobilePrev(); }
      return;
    }
    if (e.key === 'ArrowRight' || e.key === ' ') { e.preventDefault(); flipNext(); }
    else if (e.key === 'ArrowLeft') { e.preventDefault(); flipPrev(); }
  }

  function onTouchStart(e: TouchEvent) { touchStartX = e.touches[0].clientX; }
  function onTouchEnd(e: TouchEvent) {
    const dx = e.changedTouches[0].clientX - touchStartX;
    if (Math.abs(dx) > 60) { dx < 0 ? flipNext() : flipPrev(); }
  }

  function handleCurlComplete() {}

  // ── Mobile card navigation ──
  function mobileNext() {
    if (animating || mobileCard >= totalMobileCards - 1) return;
    animating = true;
    slideDir = 'next';
    setTimeout(() => {
      mobileCard++;
      slideDir = null;
      animating = false;
    }, 340);
  }

  function mobilePrev() {
    if (animating || mobileCard <= 0) return;
    animating = true;
    slideDir = 'prev';
    setTimeout(() => {
      mobileCard--;
      slideDir = null;
      animating = false;
    }, 340);
  }

  // Mobile swipe handlers
  function mTouchStart(e: TouchEvent) {
    if (animating) return;
    mTouchStartX = e.touches[0].clientX;
    mTouchStartY = e.touches[0].clientY;
    mSwiping = false;
    mSwipeX = 0;
  }

  function mTouchMove(e: TouchEvent) {
    if (animating) return;
    const dx = e.touches[0].clientX - mTouchStartX;
    const dy = e.touches[0].clientY - mTouchStartY;

    // Only start swiping if horizontal movement dominates
    if (!mSwiping && Math.abs(dx) > 10 && Math.abs(dx) > Math.abs(dy)) {
      mSwiping = true;
    }

    if (mSwiping) {
      e.preventDefault();
      // Clamp swipe: resist at edges
      if ((dx > 0 && mobileCard === 0) || (dx < 0 && mobileCard >= totalMobileCards - 1)) {
        mSwipeX = dx * 0.2; // rubber-band
      } else {
        mSwipeX = dx;
      }
    }
  }

  function mTouchEnd() {
    if (!mSwiping) { mSwipeX = 0; return; }
    const threshold = 60;
    if (mSwipeX < -threshold && mobileCard < totalMobileCards - 1) {
      mobileNext();
    } else if (mSwipeX > threshold && mobileCard > 0) {
      mobilePrev();
    }
    mSwiping = false;
    mSwipeX = 0;
  }

  onMount(() => {
    // Detect mobile
    const mq = window.matchMedia('(max-width: 720px)');
    isMobile = mq.matches;
    const handler = (e: MediaQueryListEvent) => { isMobile = e.matches; };
    mq.addEventListener('change', handler);

    window.addEventListener('keydown', handleKey);
    return () => {
      window.removeEventListener('keydown', handleKey);
      mq.removeEventListener('change', handler);
    };
  });
</script>

{#if isMobile}
  <!-- ═══════════════ MOBILE EXPERIENCE ═══════════════ -->

  {#if !mobileAccepted}
    <!-- Disclaimer gate -->
    <div class="mobile-gate">
      <div class="gate-content">
        <p class="gate-label">provBookR</p>
        <h2 class="gate-title">Best experienced on desktop</h2>
        <p class="gate-body">
          This interactive book was designed for a two-page spread layout with
          page-curl animations. The mobile version uses a simplified card view.
        </p>
        <button class="gate-btn" onclick={() => { mobileAccepted = true; }}>
          Continue anyway →
        </button>
        <p class="gate-fine">You can always come back on a larger screen.</p>
      </div>
    </div>
  {:else}
    <!-- Mobile card view -->
    <!-- svelte-ignore a11y_no_static_element_interactions -->
    <div
      class="mobile-deck"
      ontouchstart={mTouchStart}
      ontouchmove={mTouchMove}
      ontouchend={mTouchEnd}
    >
      {#each mobilePages as Page, i}
        {@const offset = i - mobileCard}
        {@const isActive = offset === 0}
        {@const isNext = offset === 1}
        {@const isPrev = offset === -1}
        {@const isNearby = Math.abs(offset) <= 1}

        {#if isNearby}
          <div
            class="mobile-card"
            class:card-active={isActive}
            class:card-next={isNext}
            class:card-prev={isPrev}
            class:slide-out-left={isActive && slideDir === 'next'}
            class:slide-in-right={isNext && slideDir === 'next'}
            class:slide-out-right={isActive && slideDir === 'prev'}
            class:slide-in-left={isPrev && slideDir === 'prev'}
            style={mSwiping && isActive ? `transform: translateX(${mSwipeX}px) scale(${1 - Math.abs(mSwipeX) * 0.0003});` : ''}
          >
            <div
              class="mobile-card-inner"
              style="font-size:var(--reading-size);font-family:var(--reading-font);line-height:var(--reading-lh);"
            >
              <!-- We assume a global provData import for the template, or fallback to passing an empty object if undefined -->
              <Page provData={typeof provData !== 'undefined' ? provData : {}} />
            </div>
          </div>
        {/if}
      {/each}

      <!-- Progress dots -->
      <div class="mobile-dots">
        {#each mobilePages as _, i}
          <button
            class="dot"
            class:active={i === mobileCard}
            onclick={() => { if (!animating) mobileCard = i; }}
            aria-label="Go to page {i + 1}"
          ></button>
        {/each}
      </div>
    </div>

    <!-- Mobile bottom bar -->
    <Dock>
      {#snippet leftControls()}
        <button 
          class="dock-btn" 
          class:active={activeScenarioState.isOpen} 
          onclick={() => activeScenarioState.isOpen = !activeScenarioState.isOpen} 
          aria-label="Notebook Scenarios" 
          title="Choose Notebook"
        >
          <Icon name="book" size={18} />
        </button>
        <button class="dock-btn" onclick={mobilePrev} disabled={mobileCard === 0 || animating} aria-label="Previous Page">
          <svg xmlns="http://www.w3.org/2000/svg" width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polyline points="15 18 9 12 15 6"></polyline></svg>
        </button>
      {/snippet}
      {#snippet rightControls()}
        <button class="dock-btn" onclick={mobileNext} disabled={mobileCard >= totalMobileCards - 1 || animating} aria-label="Next Page">
          <svg xmlns="http://www.w3.org/2000/svg" width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polyline points="9 18 15 12 9 6"></polyline></svg>
        </button>
        <span class="page-indicator">{mobileCard + 1} / {totalMobileCards}</span>
      {/snippet}
    </Dock>
  {/if}

{:else}
  <!-- ═══════════════ DESKTOP EXPERIENCE ═══════════════ -->

  <!-- svelte-ignore a11y_click_events_have_key_events -->
  <!-- svelte-ignore a11y_no_static_element_interactions -->
  <div
    class="book"
    class:cover-mode={isCover}
    ontouchstart={onTouchStart}
    ontouchend={onTouchEnd}
  >
    {#if isCover && coverPage}
      <div class="cover-page" onclick={flipNext}>
        {@render coverPage()}
      </div>
    {:else}
      <!-- Left page -->
      <div
        class="half left"
        class:justified
        style="font-size:var(--reading-size);font-family:var(--reading-font);line-height:var(--reading-lh);"
      >
        <div 
          class="page-inner"
          bind:this={leftHalf}
          onscroll={checkScrollState}
        >
          <div class="page-content-wrapper">
            {@render leftPage()}
          </div>
          <div class="page-number">{(currentSpread - 1) * 2 + 1}</div>
          {#if leftCanScroll}
            <button 
              class="scroll-indicator" 
              onclick={scrollLeftDown}
              aria-label="Scroll down for more content"
            >
              <span>Scroll for more</span>
              <Icon name="chevron-down" size={14} />
            </button>
          {/if}
        </div>
        <button class="page-tap left-tap" onclick={flipPrev} aria-label="Tap to go back" tabindex="-1"></button>
      </div>

      <div class="spine"></div>

      <!-- Right page -->
      <div
        class="half right"
        class:justified
        style="font-size:var(--reading-size);font-family:var(--reading-font);line-height:var(--reading-lh);"
      >
        <div 
          class="page-inner"
          bind:this={rightHalf}
          onscroll={checkScrollState}
        >
          <div class="page-content-wrapper">
            {@render rightPage()}
          </div>
          <div class="page-number">{(currentSpread - 1) * 2 + 2}</div>
          {#if rightCanScroll}
            <button 
              class="scroll-indicator" 
              onclick={scrollRightDown}
              aria-label="Scroll down for more content"
            >
              <span>Scroll for more</span>
              <Icon name="chevron-down" size={14} />
            </button>
          {/if}
        </div>
        <button class="page-tap right-tap" onclick={flipNext} aria-label="Tap to advance" tabindex="-1"></button>
      </div>
    {/if}
  </div>

  <!-- Three.js curl overlay -->
  {#if showCurl}
    <PageCurl
      bind:this={curlRef}
      direction={flipDirection}
      radius={curlRadius}
      duration={curlSpeed}
      mode={curlMode}
      sweep={resolveSweep(curlSweep, flipDirection)}
      onComplete={handleCurlComplete}
    />
  {/if}

  <!-- Desktop bottom bar -->
  <Dock>
    {#snippet leftControls()}
      <button 
        class="dock-btn" 
        class:active={activeScenarioState.isOpen} 
        onclick={() => activeScenarioState.isOpen = !activeScenarioState.isOpen} 
        aria-label="Notebook Scenarios" 
        title="Choose Notebook"
      >
        <Icon name="book" size={15} />
      </button>
      <button class="dock-btn" onclick={flipPrev} disabled={currentSpread === 0 || flipping} aria-label="Previous Page">
        <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polyline points="15 18 9 12 15 6"></polyline></svg>
      </button>
    {/snippet}
    {#snippet rightControls()}
      <button class="dock-btn" onclick={flipNext} disabled={currentSpread >= totalSpreads - 1 || flipping} aria-label="Next Page">
        <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polyline points="9 18 15 12 9 6"></polyline></svg>
      </button>
      <span class="page-indicator">{currentSpread === 0 ? 'Cover' : `Sp ${currentSpread}`}</span>
    {/snippet}
  </Dock>
{/if}

<style>
  /* ══════════════════════════════════════════
     DESKTOP STYLES
     ══════════════════════════════════════════ */

  /* ── Book container (The dark table) ── */
  .book {
    position: fixed;
    inset: 0;
    display: flex;
    align-items: center;
    justify-content: center;
    background: #0a0a0a;
    padding: 1rem 2rem 5rem 2rem;
  }

  /* ── Cover ── */
  .cover-page {
    height: calc(100vh - 6rem);
    max-height: 1100px;
    aspect-ratio: 1 / 1.38;
    max-width: 90vw;
    display: flex;
    align-items: center;
    justify-content: center;
    background: var(--page-bg, #f7f6f1);
    background-image: url("data:image/svg+xml,%3Csvg viewBox='0 0 200 200' xmlns='http://www.w3.org/2000/svg'%3E%3Cfilter id='noiseFilter'%3E%3CfeTurbulence type='fractalNoise' baseFrequency='0.85' numOctaves='3' stitchTiles='stitch'/%3E%3C/filter%3E%3Crect width='100%25' height='100%25' filter='url(%23noiseFilter)' opacity='0.04'/%3E%3C/svg%3E");
    overflow-y: auto;
    cursor: pointer;
    box-shadow: 0 25px 50px -12px rgba(0,0,0,0.5);
    border-radius: 4px;
    color: var(--text);
  }

  /* ── Page halves (The book block) ── */
  .half {
    height: calc(100vh - 6rem);
    max-height: 1100px;
    aspect-ratio: 1 / 1.38;
    max-width: 48vw;
    position: relative;
    background-color: var(--page-bg, #f7f6f1);
    background-image: url("data:image/svg+xml,%3Csvg viewBox='0 0 200 200' xmlns='http://www.w3.org/2000/svg'%3E%3Cfilter id='noiseFilter'%3E%3CfeTurbulence type='fractalNoise' baseFrequency='0.85' numOctaves='3' stitchTiles='stitch'/%3E%3C/filter%3E%3Crect width='100%25' height='100%25' filter='url(%23noiseFilter)' opacity='0.04'/%3E%3C/svg%3E");
    overflow: hidden;
    color: var(--text-body, var(--text));
  }


  /* ── Bleed-through text effect (from reference image) ── */
  .half::before {
    content: '';
    position: absolute;
    inset: 0;
    pointer-events: none;
    z-index: 0;
    opacity: var(--bleed-opacity, 0.035);
    background-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='400' height='200'%3E%3Cstyle%3Etext%7Bfont-family:serif;font-size:12px;fill:%23888;%7D%3C/style%3E%3Cg transform='scale(-1, 1) translate(-400, 0)'%3E%3Ctext x='10' y='20'%3ESome faint text bleeding through from the other side of the page.%3C/text%3E%3Ctext x='10' y='45'%3EIt gives a realistic print texture like a physical book.%3C/text%3E%3Ctext x='10' y='70'%3EThe opacity is kept very low so it doesn't distract.%3C/text%3E%3Ctext x='10' y='95'%3EJust enough to trick the eye into seeing physical paper.%3C/text%3E%3Ctext x='10' y='120'%3EThis matches the reference image perfectly.%3C/text%3E%3Ctext x='10' y='145'%3EAnother line of text for the bleed through effect.%3C/text%3E%3Ctext x='10' y='170'%3EAnd one more line to fill the pattern.%3C/text%3E%3C/g%3E%3C/svg%3E");
    background-repeat: repeat;
  }

  .half.justified :global(p),
  .half.justified :global(.note) {
    text-align: justify;
    hyphens: auto;
    -webkit-hyphens: auto;
  }

  .left {
    border-top-left-radius: 6px;
    border-bottom-left-radius: 6px;
    /* Spine gradient + stacked page edges on the left + drop shadow */
    box-shadow: 
      inset -25px 0 40px -15px var(--spine-shadow, rgba(0,0,0,0.12)),
      -1px 1px 0px var(--page-edge-1, #e4e3de), -2px 2px 0px var(--page-edge-2, #eeebe6), -3px 3px 0px var(--page-edge-1, #e4e3de), -4px 4px 0px var(--page-edge-2, #eeebe6), -5px 5px 0px var(--page-edge-3, #d4d3ce),
      var(--page-shadow, -20px 25px 40px rgba(0,0,0,0.5));
  }

  .right {
    border-top-right-radius: 6px;
    border-bottom-right-radius: 6px;
    /* Spine gradient + stacked page edges on the right + drop shadow */
    box-shadow: 
      inset 25px 0 40px -15px var(--spine-shadow, rgba(0,0,0,0.12)),
      1px 1px 0px var(--page-edge-1, #e4e3de), 2px 2px 0px var(--page-edge-2, #eeebe6), 3px 3px 0px var(--page-edge-1, #e4e3de), 4px 4px 0px var(--page-edge-2, #eeebe6), 5px 5px 0px var(--page-edge-3, #d4d3ce),
      var(--page-shadow, 20px 25px 40px rgba(0,0,0,0.5));
  }

  /* ── Floating Scroll Indicator ── */
  .scroll-indicator {
    all: unset;
    position: sticky;
    bottom: 1.25rem;
    left: 50%;
    transform: translateX(-50%);
    z-index: 20;
    display: inline-flex;
    align-items: center;
    gap: 0.4rem;
    padding: 0.45rem 0.9rem;
    background: var(--glass-bg, rgba(255, 255, 255, 0.88));
    backdrop-filter: blur(16px) saturate(1.4);
    border: 1px solid var(--glass-border, rgba(0, 0, 0, 0.12));
    border-radius: 999px;
    color: var(--text-secondary, #4b5563);
    font-family: var(--font-sans);
    font-size: 0.72rem;
    font-weight: 600;
    letter-spacing: 0.02em;
    cursor: pointer;
    box-shadow: 
      0 6px 16px rgba(0, 0, 0, 0.12),
      0 0 0 1px rgba(255, 255, 255, 0.6) inset;
    transition: all 0.22s cubic-bezier(0.16, 1, 0.3, 1);
    animation: fadeInPill 0.3s ease-out forwards;
    align-self: center;
    margin-top: auto;
  }

  .scroll-indicator:hover {
    background: var(--pill-badge-bg, #1a1a24);
    color: var(--pill-badge-text, #ffffff);
    transform: translateX(-50%) translateY(-2px);
    box-shadow: 0 8px 22px rgba(0, 0, 0, 0.2);
  }

  .scroll-indicator :global(svg) {
    animation: bounceDown 1.8s infinite cubic-bezier(0.45, 0, 0.55, 1);
  }

  @keyframes fadeInPill {
    from { opacity: 0; transform: translateX(-50%) translateY(8px); }
    to { opacity: 1; transform: translateX(-50%) translateY(0); }
  }

  @keyframes bounceDown {
    0%, 100% { transform: translateY(0); }
    50% { transform: translateY(3px); }
  }

  /* ── Page inner — generous notebook-like margins ── */
  .page-inner {
    position: relative;
    z-index: 1;
    width: 100%;
    height: 100%;
    max-width: 760px;
    margin: 0 auto;
    padding: clamp(1.5rem, 3.5vh, 2.5rem) clamp(1.5rem, 3vw, 2.5rem);
    overflow-y: auto;
    overflow-x: hidden;
    box-sizing: border-box;
    display: flex;
    flex-direction: column;
  }

  .page-content-wrapper {
    width: 100%;
    margin: auto 0;
    padding-top: 1.8rem;
    display: flex;
    flex-direction: column;
  }

  /* ── Page numbers ── */
  .page-number {
    position: absolute;
    bottom: 1rem;
    left: 50%;
    transform: translateX(-50%);
    font-family: var(--font-mono, monospace);
    font-size: 0.65rem;
    color: var(--text-muted, #94a3b8);
    text-align: center;
    letter-spacing: 0.05em;
    pointer-events: none;
  }

  /* ── Click-to-navigate tap targets (behind content) ── */
  .page-tap {
    all: unset;
    position: absolute;
    inset: 0;
    z-index: 0;
    cursor: pointer;
  }

  /* ── Spine ── */
  .spine {
    width: 2px;
    height: 100%;
    max-height: 1300px;
    flex-shrink: 0;
    background: linear-gradient(
      to bottom,
      rgba(0,0,0,0) 0%,
      rgba(0,0,0,0.1) 2%,
      rgba(0,0,0,0.2) 5%,
      rgba(0,0,0,0.2) 95%,
      rgba(0,0,0,0.1) 98%,
      rgba(0,0,0,0) 100%
    );
    z-index: 2;
    box-shadow:
      -4px 0 12px rgba(0,0,0,0.05),
       4px 0 12px rgba(0,0,0,0.05);
  }



  /* ══════════════════════════════════════════
     MOBILE — Disclaimer gate
     ══════════════════════════════════════════ */

  .mobile-gate {
    position: fixed;
    inset: 0;
    display: flex;
    align-items: center;
    justify-content: center;
    background: var(--bg-warm, #faf9f7);
    z-index: 500;
    padding: 2rem;
  }

  .gate-content {
    text-align: center;
    max-width: 340px;
  }

  .gate-label {
    font-family: var(--font-sans);
    font-size: 0.65rem;
    font-weight: 600;
    text-transform: uppercase;
    letter-spacing: 0.12em;
    color: var(--text-muted, #94a3b8);
    margin-bottom: 1rem;
  }

  .gate-title {
    font-size: 1.6rem;
    letter-spacing: -0.02em;
    margin-bottom: 1rem;
    line-height: 1.25;
  }

  .gate-body {
    font-family: var(--font-sans);
    font-size: 0.88rem;
    color: var(--text-secondary, #555);
    line-height: 1.55;
    margin-bottom: 2rem;
  }

  .gate-btn {
    font-family: var(--font-sans);
    font-size: 0.85rem;
    font-weight: 500;
    padding: 0.65rem 1.8rem;
    border: 1px solid var(--accent, #2563eb);
    border-radius: 6px;
    background: var(--accent, #2563eb);
    color: #fff;
    cursor: pointer;
    transition: background 0.15s, transform 0.1s;
  }
  .gate-btn:hover { background: #1d4ed8; }
  .gate-btn:active { transform: scale(0.97); }

  .gate-fine {
    font-family: var(--font-sans);
    font-size: 0.72rem;
    color: var(--text-muted, #94a3b8);
    margin-top: 1.2rem;
  }

  /* ══════════════════════════════════════════
     MOBILE — Card deck
     ══════════════════════════════════════════ */

  .mobile-deck {
    position: fixed;
    inset: 0;
    bottom: 44px;
    overflow: hidden;
    background: var(--bg-warm, #faf9f7);
    touch-action: pan-y;
  }

  .mobile-card {
    position: absolute;
    inset: 0;
    background: var(--page-bg, #fff);
    color: var(--text-body, var(--text));
    border-radius: 0;
    overflow-y: auto;
    overflow-x: hidden;
    -webkit-overflow-scrolling: touch;
    will-change: transform, opacity;
    transition: transform 0.34s cubic-bezier(0.4, 0, 0.2, 1),
                opacity 0.34s cubic-bezier(0.4, 0, 0.2, 1);
  }

  /* Positioning: active card is visible, neighbors are off-screen */
  .card-active {
    transform: translateX(0) scale(1);
    opacity: 1;
    z-index: 2;
  }

  .card-next {
    transform: translateX(100%) scale(0.95);
    opacity: 0.6;
    z-index: 1;
  }

  .card-prev {
    transform: translateX(-100%) scale(0.95);
    opacity: 0.6;
    z-index: 1;
  }

  /* Slide animations */
  .slide-out-left {
    transform: translateX(-100%) scale(0.95) !important;
    opacity: 0.4 !important;
  }

  .slide-in-right {
    transform: translateX(0) scale(1) !important;
    opacity: 1 !important;
  }

  .slide-out-right {
    transform: translateX(100%) scale(0.95) !important;
    opacity: 0.4 !important;
  }

  .slide-in-left {
    transform: translateX(0) scale(1) !important;
    opacity: 1 !important;
  }

  .mobile-card-inner {
    max-width: 100%;
    padding: 0 1.5rem;
  }

  /* Tighten page content padding on mobile cards */
  .mobile-card-inner :global(.spread) {
    min-height: auto;
    padding: 2rem 0;
  }

  .mobile-card-inner :global(.page) {
    max-width: 100%;
    padding: 0;
  }

  .mobile-card-inner :global(.page-inner) {
    padding: 2rem 0 3rem;
    max-width: 100%;
  }

  .mobile-card-inner :global(section) {
    padding: 0;
  }

  .mobile-card-inner :global(section > .page) {
    padding: 0;
  }

  .mobile-card-inner :global(figure) {
    margin-left: -0.5rem;
    margin-right: -0.5rem;
    overflow: hidden;
  }

  .mobile-card-inner :global(figure canvas) {
    max-width: 100%;
    height: auto;
  }

  .mobile-card-inner :global(.layer-stack),
  .mobile-card-inner :global(.layer) {
    max-width: 100%;
  }

  .mobile-card-inner :global(.note) {
    word-break: break-word;
  }

  .mobile-card-inner :global(.cover-content) {
    padding-left: 0;
    padding-right: 0;
  }

  /* ── Progress dots ── */
  .mobile-dots {
    position: absolute;
    bottom: 8px;
    left: 50%;
    transform: translateX(-50%);
    display: flex;
    gap: 6px;
    z-index: 10;
  }

  .dot {
    width: 6px;
    height: 6px;
    border-radius: 50%;
    border: none;
    background: var(--border, #e5e7eb);
    cursor: pointer;
    padding: 0;
    transition: background 0.2s, transform 0.2s;
  }
  .dot.active {
    background: var(--accent, #2563eb);
    transform: scale(1.3);
  }

</style>

<script lang="ts">
  import PageFlip from '$lib/components/PageFlip.svelte';
  import BookGraphView from '$lib/components/BookGraphView.svelte';
  import GuideOverlay from '$lib/components/GuideOverlay.svelte';
  
  import ProvCover from '$lib/components/prov_pages/ProvCover.svelte';
  import IntroLeft from '$lib/components/prov_pages/IntroLeft.svelte';
  import IntroRight from '$lib/components/prov_pages/IntroRight.svelte';
  import CodeLeft from '$lib/components/prov_pages/CodeLeft.svelte';
  import CodeRight from '$lib/components/prov_pages/CodeRight.svelte';
  import GraphLeft from '$lib/components/prov_pages/GraphLeft.svelte';
  import GraphRight from '$lib/components/prov_pages/GraphRight.svelte';
  import LineageLeft from '$lib/components/prov_pages/LineageLeft.svelte';
  import LineageRight from '$lib/components/prov_pages/LineageRight.svelte';
  import OutputLeft from '$lib/components/prov_pages/OutputLeft.svelte';
  import OutputRight from '$lib/components/prov_pages/OutputRight.svelte';
  import BlankPage from '$lib/components/prov_pages/BlankPage.svelte';

  import { activeScenarioState, viewModeState, bookNavigationState } from '$lib/state.svelte';
  import { scenarios } from '$lib/data/scenarios';
  import scriptSource from '$lib/data/script_source.json';
  import { parseProvData } from '$lib/data/provParser';

  let provData = $derived(scenarios[activeScenarioState.currentId].provData);
  let isBlank = $derived(activeScenarioState.currentId === 'blank');

  const parsed = $derived(provData ? parseProvData(provData, scriptSource) : null);
  const scriptName = $derived(parsed?.scriptName || (scriptSource as any)?.name || 'script.R');
  const pageTitle = $derived(`provBookR: ${scriptName}`);

  const contentSpreads = [
    [IntroLeft, IntroRight],       // Chapter 1: Everyday Provenance
    [CodeLeft, CodeRight],         // Chapter 2: Source Code
    [GraphLeft, GraphRight],       // Chapter 3: Lineage Graph
    [LineageLeft, LineageRight],   // Chapter 4: Data Lineage & Artifacts
    [OutputLeft, OutputRight]      // Chapter 5: Execution Timeline & Derivation
  ];

  const totalSpreads = 1 + contentSpreads.length;
  
  function next() { bookNavigationState.currentSpread = Math.min(bookNavigationState.currentSpread + 1, totalSpreads - 1); }
  function prev() { bookNavigationState.currentSpread = Math.max(bookNavigationState.currentSpread - 1, 0); }
</script>

<svelte:head>
  <title>{pageTitle}</title>
</svelte:head>

{#if viewModeState.mode === 'graph'}
  <BookGraphView 
    spreads={[
      { cover: ProvCover, label: 'Cover' },
      { left: IntroLeft, right: IntroRight, label: 'Introduction' },
      { left: CodeLeft, right: CodeRight, label: 'Source Code' },
      { left: GraphLeft, right: GraphRight, label: 'Lineage Graph' },
      { left: LineageLeft, right: LineageRight, label: 'Data Flow' },
      { left: OutputLeft, right: OutputRight, label: 'Execution' }
    ]} 
    {provData} 
  />
{:else}
  <div class="page-flip-container">
    <PageFlip
      currentSpread={bookNavigationState.currentSpread}
      {totalSpreads}
    onNext={next}
    onPrev={prev}
    isCover={bookNavigationState.currentSpread === 0}
    mobilePages={isBlank ? [BlankPage, BlankPage, BlankPage, BlankPage, BlankPage, BlankPage] : [
      ProvCover, 
      IntroLeft, IntroRight, 
      CodeLeft, CodeRight, 
      GraphLeft, GraphRight, 
      LineageLeft, LineageRight, 
      OutputLeft, OutputRight
    ]}
  >
    {#snippet coverPage()}
      {#if isBlank}
        <BlankPage pageKey="blank-cover" />
      {:else}
        <ProvCover {provData} />
      {/if}
    {/snippet}

    {#snippet leftPage()}
      {#if isBlank}
        <BlankPage pageKey={`blank-spread-${bookNavigationState.currentSpread}-left`} />
      {:else if bookNavigationState.currentSpread > 0}
        {@const Left = contentSpreads[bookNavigationState.currentSpread - 1][0]}
        {#if Left}
          <Left {provData} />
        {/if}
      {/if}
    {/snippet}

    {#snippet rightPage()}
      {#if isBlank}
        <BlankPage pageKey={`blank-spread-${bookNavigationState.currentSpread}-right`} />
      {:else if bookNavigationState.currentSpread > 0}
        {@const Right = contentSpreads[bookNavigationState.currentSpread - 1][1]}
        {#if Right}
          <Right {provData} />
        {/if}
      {/if}
    {/snippet}
  </PageFlip>
  </div>
  <GuideOverlay />
{/if}

<style>
  .page-flip-container {
    width: 100vw;
    height: 100vh;
    display: block;
    position: relative;
  }
</style>

<script lang="ts">
  import '$lib/styles/global.css';
  import { themeState } from '$lib/themeState.svelte';
  import { viewModeState } from '$lib/state.svelte';
  import SettingsPanel from '$lib/components/SettingsPanel.svelte';
  import AnnotationsPanel from '$lib/components/AnnotationsPanel.svelte';
  import ScenarioPanel from '$lib/components/ScenarioPanel.svelte';
  import Dock from '$lib/components/Dock.svelte';
  
  let { children } = $props();
</script>

<div class="theme-wrapper {themeState.skin} {themeState.codeFont}">
  {@render children()}
  <SettingsPanel />
  <AnnotationsPanel />
  <ScenarioPanel />
</div>

<style>
  .theme-wrapper {
    min-height: 100vh;
    display: flex;
    flex-direction: column;
    background: var(--bg-warm);
    color: var(--text);
  }
</style>



export const themeState = $state({
  codeFont: 'font-system-mono', // 'font-system-mono', 'font-fira-code', 'font-jetbrains', 'font-sans-code'
  skin: 'theme-eink-light',     // 'theme-eink-light', 'theme-eink-dark', 'theme-high-contrast'
  isOpen: false,                 // UI toggle for the settings panel
  
  // Typography/Reading Settings
  readingMode: 'light' as 'detailed' | 'light', // 'detailed' = full provenance data dump, 'light' = sparse, curated book reading
  fontSize: 12.5,
  fontFamily: 'sans',
  lineHeight: 1.78,
  justified: true,
  coverStyle: 'modern' as 'modern' | 'classic',

  // Page Turn Settings
  curlEnabled: false,
  curlRadius: 0.14,
  curlSpeed: 1.2,
  curlMode: 'ripple' as 'curl' | 'ripple',
  curlSweep: 'corner' as import('./components/pageCurlTypes').SweepStyle
});

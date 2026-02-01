# Navigation Button Component

## Overview

A Floating Action Button (FAB) that provides navigation across the application while preserving screen real estate for the main game view.

## Component Location

`src/components/common/NavigationButton.vue`

## Props

| Prop | Type | Required | Default | Description |
|------|------|----------|---------|-------------|
| `isLoggedIn` | `boolean` | Yes | - | Determines whether to show Login or Logout option |
| `position` | `'fixed' \| 'inline'` | No | `'fixed'` | Controls positioning: `fixed` for bottom-left FAB, `inline` for toolbar use |

## Navigation Items

The FAB expands to reveal the following navigation options:

| Label | Icon | Route | Condition |
|-------|------|-------|-----------|
| Home | `pi-home` | `/` | Always visible |
| Create Game | `pi-plus` | `/create-lobby` | Always visible |
| Login | `pi-sign-in` | `/auth/login` | When `isLoggedIn` is `false` |
| Logout | `pi-sign-out` | `/auth/logout` | When `isLoggedIn` is `true` |

## Behavior

1. **Collapsed State**: Displays a hamburger menu icon (`pi-bars`)
2. **Expanded State**:
   - Icon changes to close (`pi-times`) with rotation animation
   - Menu items animate in from bottom with staggered delay
   - Each item shows an icon button + text label
3. **Click Outside**: Menu closes when clicking outside the FAB container
4. **Navigation**: Clicking a menu item navigates via `window.location.href` and closes the menu

## Positioning

### Fixed mode (`position="fixed"`)
- Fixed to bottom-left corner (`bottom-4 left-4`)
- Menu expands upward
- Best for desktop use

### Inline mode (`position="inline"`)
- Relative positioning, flows with parent layout
- Menu expands downward
- Best for use in toolbars (e.g., mobile top bar)

## Usage

```vue
<script setup lang="ts">
import NavigationButton from '@/components/common/NavigationButton.vue'

const isLoggedIn = ref(true)
</script>

<template>
  <!-- Desktop: Fixed FAB in bottom-left -->
  <NavigationButton :is-logged-in="isLoggedIn" class="hidden lg:block" />

  <!-- Mobile: Inline in toolbar, menu expands downward -->
  <div class="lg:hidden toolbar">
    <NavigationButton :is-logged-in="isLoggedIn" position="inline" />
    <OtherToolbarContent />
  </div>
</template>
```

## Integration with createRound

The `isLoggedIn` prop should be passed through the `createRound` entrypoint:

```typescript
import { createRound } from 'wordify-views'

const round = createRound('#app', {
  initialState: gameState,
  websocketUrl: 'wss://...',
  isLoggedIn: true
})
```

## Accessibility

- Main FAB has `aria-label="Navigation menu"`
- Each menu item button has `aria-label` matching the item label
- Keyboard navigation supported via native button focus

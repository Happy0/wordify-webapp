<script setup lang="ts">
import { computed, ref } from 'vue'
import { useGameStore } from '@/stores/gameStore'
import { storeToRefs } from 'pinia'
import Button from 'primevue/button'
import Dialog from 'primevue/dialog'
import GameTile from './GameTile.vue'

const emit = defineEmits<{
  (e: 'submit'): void
  (e: 'exchange', tileIds: string[]): void
  (e: 'pass'): void
}>()

const store = useGameStore()
const { rack, isMyTurn, candidateTilesOnBoard } = storeToRefs(store)

const exchangeMode = ref(false)
const selectedForExchange = ref<Set<string>>(new Set())

const hasPlacedTiles = computed(() => candidateTilesOnBoard.value.length > 0)

const canSubmit = computed(() => isMyTurn.value && hasPlacedTiles.value)
const canExchange = computed(() => isMyTurn.value && !hasPlacedTiles.value && rack.value.length > 0)
const canPass = computed(() => isMyTurn.value && !hasPlacedTiles.value)

function handleSubmit() {
  if (canSubmit.value) {
    emit('submit')
  }
}

function openExchangeDialog() {
  if (canExchange.value) {
    selectedForExchange.value = new Set()
    exchangeMode.value = true
  }
}

function toggleTileForExchange(tileId: string) {
  if (selectedForExchange.value.has(tileId)) {
    selectedForExchange.value.delete(tileId)
  } else {
    selectedForExchange.value.add(tileId)
  }
  // Force reactivity
  selectedForExchange.value = new Set(selectedForExchange.value)
}

function confirmExchange() {
  if (selectedForExchange.value.size > 0) {
    emit('exchange', Array.from(selectedForExchange.value))
    exchangeMode.value = false
    selectedForExchange.value = new Set()
  }
}

function cancelExchange() {
  exchangeMode.value = false
  selectedForExchange.value = new Set()
}

function handlePass() {
  if (canPass.value) {
    emit('pass')
  }
}
</script>

<template>
  <div class="move-controls flex gap-2 justify-center">
    <Button
      label="Submit"
      icon="pi pi-check"
      severity="success"
      :disabled="!canSubmit"
      @click="handleSubmit"
    />
    <Button
      label="Exchange"
      icon="pi pi-sync"
      severity="info"
      :disabled="!canExchange"
      @click="openExchangeDialog"
    />
    <Button
      label="Pass"
      icon="pi pi-forward"
      severity="secondary"
      :disabled="!canPass"
      @click="handlePass"
    />

    <!-- Exchange Dialog -->
    <Dialog
      v-model:visible="exchangeMode"
      header="Select tiles to exchange"
      :modal="true"
      :closable="true"
      :draggable="false"
      class="w-80"
    >
      <p class="text-sm text-gray-600 mb-4">
        Click on the tiles you want to exchange, then confirm.
      </p>

      <div class="flex flex-wrap gap-2 justify-center mb-4">
        <div
          v-for="tile in rack"
          :key="tile.id"
          class="cursor-pointer transition-transform"
          :class="{
            'ring-2 ring-blue-500 ring-offset-2 rounded': selectedForExchange.has(tile.id)
          }"
          @click="toggleTileForExchange(tile.id)"
        >
          <GameTile :tile="tile" size="large" />
        </div>
      </div>

      <div class="flex gap-2 justify-end">
        <Button
          label="Cancel"
          severity="secondary"
          size="small"
          @click="cancelExchange"
        />
        <Button
          label="Exchange"
          severity="success"
          size="small"
          :disabled="selectedForExchange.size === 0"
          @click="confirmExchange"
        />
      </div>
    </Dialog>
  </div>
</template>

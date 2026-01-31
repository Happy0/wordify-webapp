// Touch-friendly drag and drop composable
import { ref, onUnmounted } from 'vue'

export type DragState = {
  isDragging: boolean
  isPending: boolean // Touch started but not yet moved enough to drag
  tileId: string | null
  startX: number
  startY: number
  currentX: number
  currentY: number
  element: HTMLElement | null
  clone: HTMLElement | null
}

// Movement threshold in pixels before drag starts
const DRAG_THRESHOLD = 10

const dragState = ref<DragState>({
  isDragging: false,
  isPending: false,
  tileId: null,
  startX: 0,
  startY: 0,
  currentX: 0,
  currentY: 0,
  element: null,
  clone: null
})

let dropCallback: ((row: number, col: number) => void) | null = null
let cancelCallback: (() => void) | null = null
let tapCallback: ((tileId: string) => void) | null = null

function createDragClone(element: HTMLElement, x: number, y: number): HTMLElement {
  const clone = element.cloneNode(true) as HTMLElement
  clone.style.position = 'fixed'
  clone.style.left = `${x - 20}px`
  clone.style.top = `${y - 20}px`
  clone.style.width = '40px'
  clone.style.height = '40px'
  clone.style.zIndex = '9999'
  clone.style.pointerEvents = 'none'
  clone.style.opacity = '0.9'
  clone.style.transform = 'scale(1.2)'
  clone.style.boxShadow = '0 4px 12px rgba(0,0,0,0.3)'
  clone.style.borderRadius = '4px'
  document.body.appendChild(clone)
  return clone
}

function updateClonePosition(clone: HTMLElement, x: number, y: number) {
  clone.style.left = `${x - 20}px`
  clone.style.top = `${y - 20}px`
}

function removeClone(clone: HTMLElement | null) {
  if (clone && clone.parentNode) {
    clone.parentNode.removeChild(clone)
  }
}

function findDropTarget(x: number, y: number): { row: number; col: number } | null {
  const elementsAtPoint = document.elementsFromPoint(x, y)

  for (const el of elementsAtPoint) {
    // Check if it's a board square
    if (el.classList.contains('board-square')) {
      const row = el.getAttribute('data-row')
      const col = el.getAttribute('data-col')
      if (row !== null && col !== null) {
        return { row: parseInt(row), col: parseInt(col) }
      }
    }
  }
  return null
}

export function useTouchDragDrop() {
  // Start pending drag - records intent to drag but doesn't create visual clone yet
  function startPendingDrag(tileId: string, element: HTMLElement, clientX: number, clientY: number) {
    dragState.value = {
      isDragging: false,
      isPending: true,
      tileId,
      startX: clientX,
      startY: clientY,
      currentX: clientX,
      currentY: clientY,
      element,
      clone: null
    }
  }

  // Actually start the visual drag (called when movement threshold exceeded)
  function startDrag(tileId: string, element: HTMLElement, clientX: number, clientY: number) {
    dragState.value = {
      isDragging: true,
      isPending: false,
      tileId,
      startX: clientX,
      startY: clientY,
      currentX: clientX,
      currentY: clientY,
      element,
      clone: createDragClone(element, clientX, clientY)
    }

    // Hide original during drag
    element.style.opacity = '0.3'
  }

  function updateDrag(clientX: number, clientY: number) {
    if (!dragState.value.isDragging || !dragState.value.clone) return

    dragState.value.currentX = clientX
    dragState.value.currentY = clientY
    updateClonePosition(dragState.value.clone, clientX, clientY)
  }

  function endDrag() {
    if (!dragState.value.isDragging) return

    const { clone, element, currentX, currentY, tileId } = dragState.value

    // Restore original element opacity
    if (element) {
      element.style.opacity = '1'
    }

    // Find drop target
    const target = findDropTarget(currentX, currentY)

    if (target && dropCallback && tileId) {
      dropCallback(target.row, target.col)
    } else if (cancelCallback) {
      cancelCallback()
    }

    // Clean up
    removeClone(clone)

    dragState.value = {
      isDragging: false,
      isPending: false,
      tileId: null,
      startX: 0,
      startY: 0,
      currentX: 0,
      currentY: 0,
      element: null,
      clone: null
    }
  }

  function cancelDrag() {
    if (!dragState.value.isDragging && !dragState.value.isPending) return

    const { clone, element } = dragState.value

    if (element) {
      element.style.opacity = '1'
    }

    removeClone(clone)

    if (cancelCallback) {
      cancelCallback()
    }

    dragState.value = {
      isDragging: false,
      isPending: false,
      tileId: null,
      startX: 0,
      startY: 0,
      currentX: 0,
      currentY: 0,
      element: null,
      clone: null
    }
  }

  function setDropCallback(callback: (row: number, col: number) => void) {
    dropCallback = callback
  }

  function setCancelCallback(callback: () => void) {
    cancelCallback = callback
  }

  function setTapCallback(callback: (tileId: string) => void) {
    tapCallback = callback
  }

  // Touch event handlers
  function handleTouchStart(e: TouchEvent, tileId: string) {
    if (e.touches.length !== 1) return

    const touch = e.touches[0]
    if (!touch) return

    const element = e.currentTarget as HTMLElement

    // Prevent default to avoid scrolling
    e.preventDefault()

    // Start in pending state - actual drag starts when movement threshold exceeded
    startPendingDrag(tileId, element, touch.clientX, touch.clientY)
  }

  function handleTouchMove(e: TouchEvent) {
    if (!dragState.value.isDragging && !dragState.value.isPending) return
    if (e.touches.length !== 1) return

    e.preventDefault()

    const touch = e.touches[0]
    if (!touch) return

    // If in pending state, check if movement exceeds threshold to start drag
    if (dragState.value.isPending && !dragState.value.isDragging) {
      const dx = touch.clientX - dragState.value.startX
      const dy = touch.clientY - dragState.value.startY
      const distance = Math.sqrt(dx * dx + dy * dy)

      if (distance >= DRAG_THRESHOLD) {
        // Movement exceeded threshold - start actual drag
        const { tileId, element } = dragState.value
        if (tileId && element) {
          startDrag(tileId, element, touch.clientX, touch.clientY)
        }
      }
      return
    }

    updateDrag(touch.clientX, touch.clientY)
  }

  function handleTouchEnd(e: TouchEvent) {
    // If still in pending state (not moved enough to drag), treat as a tap
    if (dragState.value.isPending && !dragState.value.isDragging) {
      const tileId = dragState.value.tileId

      // Reset state
      dragState.value = {
        isDragging: false,
        isPending: false,
        tileId: null,
        startX: 0,
        startY: 0,
        currentX: 0,
        currentY: 0,
        element: null,
        clone: null
      }

      // Call tap callback
      if (tileId && tapCallback) {
        tapCallback(tileId)
      }
      return
    }

    if (!dragState.value.isDragging) return

    e.preventDefault()
    endDrag()
  }

  function handleTouchCancel() {
    // Reset pending state if active
    if (dragState.value.isPending && !dragState.value.isDragging) {
      dragState.value = {
        isDragging: false,
        isPending: false,
        tileId: null,
        startX: 0,
        startY: 0,
        currentX: 0,
        currentY: 0,
        element: null,
        clone: null
      }
      return
    }
    cancelDrag()
  }

  // Mouse event handlers (for consistency)
  function handleMouseDown(e: MouseEvent, tileId: string) {
    const element = e.currentTarget as HTMLElement
    startDrag(tileId, element, e.clientX, e.clientY)

    // Add document-level listeners
    document.addEventListener('mousemove', handleMouseMove)
    document.addEventListener('mouseup', handleMouseUp)
  }

  function handleMouseMove(e: MouseEvent) {
    updateDrag(e.clientX, e.clientY)
  }

  function handleMouseUp() {
    endDrag()
    document.removeEventListener('mousemove', handleMouseMove)
    document.removeEventListener('mouseup', handleMouseUp)
  }

  // Cleanup
  onUnmounted(() => {
    document.removeEventListener('mousemove', handleMouseMove)
    document.removeEventListener('mouseup', handleMouseUp)
    if (dragState.value.clone) {
      removeClone(dragState.value.clone)
    }
  })

  return {
    dragState,
    startDrag,
    updateDrag,
    endDrag,
    cancelDrag,
    setDropCallback,
    setCancelCallback,
    setTapCallback,
    handleTouchStart,
    handleTouchMove,
    handleTouchEnd,
    handleTouchCancel,
    handleMouseDown
  }
}

// Global touch move/end handlers need to be added at the document level
export function setupGlobalTouchHandlers(
  onMove: (e: TouchEvent) => void,
  onEnd: (e: TouchEvent) => void,
  onCancel: () => void
) {
  document.addEventListener('touchmove', onMove, { passive: false })
  document.addEventListener('touchend', onEnd)
  document.addEventListener('touchcancel', onCancel)

  return () => {
    document.removeEventListener('touchmove', onMove)
    document.removeEventListener('touchend', onEnd)
    document.removeEventListener('touchcancel', onCancel)
  }
}

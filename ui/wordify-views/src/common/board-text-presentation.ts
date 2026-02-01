import type { TileInput, PlacedTile } from '../types/game'
import { BOARD_SIZE } from '../types/game'

/**
 * A map of letter strings to their tile values.
 * Used to construct LetterTile objects with the appropriate values.
 */
export type TileValueMap = Record<string, number>

/**
 * Parses a tile string from the board text representation.
 * Returns a TileInput if the string represents a tile, or null if it's an empty square.
 * Returns undefined if the tile string is invalid (unrecognized letter).
 *
 * @param tileString - The string representation of the tile (e.g., "H", "[I]", or "")
 * @param tileValues - A map of letter strings to their tile values
 * @returns A TileInput object, null for empty squares, or undefined for invalid tiles
 */
function parseTileString(tileString: string, tileValues: TileValueMap): TileInput | null | undefined {
  // Empty string means no tile on this square
  if (tileString === '') {
    return null
  }

  // Check if it's a blank tile (wrapped in square brackets)
  if (tileString.startsWith('[') && tileString.endsWith(']')) {
    const assignedLetter = tileString.slice(1, -1)
    if (assignedLetter.length === 0) {
      return undefined // Invalid: empty brackets
    }
    return {
      type: 'blank',
      assigned: assignedLetter
    }
  }

  // It's a regular letter tile - look up its value
  const value = tileValues[tileString]
  if (value === undefined) {
    return undefined // Invalid: unrecognized letter
  }

  return {
    type: 'letter',
    letter: tileString,
    value
  }
}

/**
 * Converts a board text representation string to a PlacedTile[] array.
 *
 * The board text representation is a comma-delimited string where each element is:
 * - An empty string for an unoccupied square
 * - The letter string for a regular tile (e.g., "H", "QU")
 * - A letter wrapped in square brackets for a blank tile (e.g., "[I]")
 *
 * The string is ordered by column then row, starting at position A1 (column 1, row 1)
 * and ending at O15 (column 15, row 15).
 *
 * @param textRepresentation - The comma-delimited board string
 * @param tileValues - A map of letter strings to their tile values
 * @returns A PlacedTile[] array with 1-based positions, or null if the representation is invalid
 */
export function fromBoardTextRepresentation(
  textRepresentation: string,
  tileValues: TileValueMap
): PlacedTile[] | null {
  const parts = textRepresentation.split(',')

  // A valid board has exactly 225 squares (15x15)
  if (parts.length !== BOARD_SIZE * BOARD_SIZE) {
    return null
  }

  const placedTiles: PlacedTile[] = []

  for (let i = 0; i < parts.length; i++) {
    const tileString = parts[i]!

    // Skip empty squares
    if (tileString === '') {
      continue
    }

    const tile = parseTileString(tileString, tileValues)

    // undefined means invalid tile string
    if (tile === undefined) {
      return null
    }

    // null means empty square (already handled above, but for type safety)
    if (tile === null) {
      continue
    }

    // Calculate column and row from 1D index (1-based for PlacedTile positions)
    // The string is ordered by column then row:
    // Index 0 = col 1, row 1 (A1)
    // Index 1 = col 1, row 2 (A2)
    // ...
    // Index 14 = col 1, row 15 (A15)
    // Index 15 = col 2, row 1 (B1)
    const x = Math.floor(i / BOARD_SIZE) + 1
    const y = (i % BOARD_SIZE) + 1

    placedTiles.push({
      position: { x, y },
      tile
    })
  }

  return placedTiles
}

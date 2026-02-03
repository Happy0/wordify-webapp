/**
 * A map of letter strings to their tile values.
 * Used to construct LetterTile objects with the appropriate values.
 *
 * The keys can be single characters (e.g., "A", "B") or multi-character
 * strings for locales that use digraphs (e.g., Spanish "CH", "RR", "LL").
 *
 * @example
 * // English tile values
 * const englishTiles: TileValueMap = {
 *   'A': 1, 'B': 3, 'C': 3, 'D': 2, 'E': 1,
 *   // ...
 * }
 *
 * // Spanish tile values with digraphs
 * const spanishTiles: TileValueMap = {
 *   'A': 1, 'CH': 5, 'LL': 8, 'RR': 8, 'Ñ': 8,
 *   // ...
 * }
 */
export type TileValueMap = Record<string, number>

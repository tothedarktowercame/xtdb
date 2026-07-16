@file:JvmName("TextSearch")

package xtdb.text

import xtdb.bloom.BLOOM_BITS
import xtdb.bloom.BLOOM_K
import xtdb.util.Hasher
import java.nio.charset.StandardCharsets.UTF_8

/**
 * v0 analysis chain for `text_match` (#5637): lowercase, split on non-letter/digit
 * (Unicode-aware), no stemming, no stop-words.
 *
 * This is the single source of truth for text analysis: the write-side term bloom
 * ([xtdb.metadata.ColumnMetadata]) and the query-side recheck (`xtdb.expression.text`)
 * and pruning hashes (`xtdb.expression.metadata`) must all tokenize identically,
 * otherwise pruning drops pages the recheck would have matched.
 */
fun tokenize(text: String): List<String> {
    val tokens = mutableListOf<String>()
    val sb = StringBuilder()
    var i = 0
    while (i < text.length) {
        val cp = text.codePointAt(i)
        if (Character.isLetterOrDigit(cp)) {
            sb.appendCodePoint(Character.toLowerCase(cp))
        } else if (sb.isNotEmpty()) {
            tokens.add(sb.toString())
            sb.setLength(0)
        }
        i += Character.charCount(cp)
    }
    if (sb.isNotEmpty()) tokens.add(sb.toString())
    return tokens
}

/**
 * All tokens of [needle] present in [haystack] (AND semantics, no ordering/phrase).
 * A needle with no tokens matches vacuously — the same contract the pruning
 * side implements (zero term-hashes ⇒ page not pruned).
 */
fun textMatch(haystack: String, needle: String): Boolean =
    textMatchTokens(haystack, tokenize(needle))

fun textMatchTokens(haystack: String, needleTokens: List<String>): Boolean {
    if (needleTokens.isEmpty()) return true
    val haystackTokens = tokenize(haystack).toHashSet()
    return needleTokens.all { it in haystackTokens }
}

/**
 * Bloom hashes for one token, identical to what [xtdb.bloom.BloomBuilder.add]
 * inserts for the token's UTF-8 bytes on the write side.
 */
@JvmOverloads
fun tokenHashes(token: String, k: Int = BLOOM_K, bits: Int = BLOOM_BITS): IntArray {
    val mask = (1 shl bits) - 1
    val bytes = token.toByteArray(UTF_8)
    val hash1 = Hasher.Xx().hash(bytes)
    val hash2 = Hasher.Murmur3().hash(bytes)
    return IntArray(k) { n -> mask and (hash1 + hash2 * n) }
}

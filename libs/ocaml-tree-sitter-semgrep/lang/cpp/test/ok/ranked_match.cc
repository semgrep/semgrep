/*
  Copied from https://github.com/mawww/kakoune
  Public Domain.

  Modified so it could parse with the tree-sitter grammar right away:
  - 'and' -> '&&'
*/

#include "ranked_match.hh"

#include "flags.hh"
#include "unit_tests.hh"
#include "utf8_iterator.hh"
#include "optional.hh"

#include <algorithm>

namespace Kakoune
{

UsedLetters used_letters(StringView str)
{
    UsedLetters res = 0;
    for (auto c : str)
    {
        if (c >= 'a' && c <= 'z')
            res |= 1uLL << (c - 'a');
        else if (c >= 'A' && c <= 'Z')
            res |= 1uLL << (c - 'A' + 26);
        else if (c == '_')
            res |= 1uLL << 53;
        else if (c == '-')
            res |= 1uLL << 54;
        else
            res |= 1uLL << 63;
    }
    return res;
}

bool matches(UsedLetters query, UsedLetters letters)
{
    return (query & letters) == query;
}

using Utf8It = utf8::iterator<const char*>;

static int count_word_boundaries_match(StringView candidate, StringView query)
{
    int count = 0;
    Utf8It query_it{query.begin(), query};
    Codepoint prev = 0;
    for (Utf8It it{candidate.begin(), candidate}; it != candidate.end(); ++it)
    {
        const Codepoint c = *it;
        const bool is_word_boundary = prev == 0 ||
                                      (!iswalnum((wchar_t)prev) && iswalnum((wchar_t)c)) ||
                                      (iswlower((wchar_t)prev) && iswupper((wchar_t)c));
        prev = c;

        if (! is_word_boundary)
            continue;

        const Codepoint lc = to_lower(c);
        for (auto qit = query_it; qit != query.end(); ++qit)
        {
            const Codepoint qc = *qit;
            if (qc == (iswlower((wchar_t)qc) ? lc  : c))
            {
                ++count;
                query_it = qit+1;
                break;
            }
        }
        if (query_it == query.end())
            break;
    }
    return count;
}

static bool smartcase_eq(Codepoint candidate, Codepoint query)
{
    return query == (iswlower((wchar_t)query) ? to_lower(candidate) : candidate);
}

struct SubseqRes
{
    int max_index;
    bool single_word;
};

static Optional<SubseqRes> subsequence_match_smart_case(StringView str, StringView subseq)
{
    bool single_word = true;
    int max_index = -1;
    auto it = str.begin();
    int index = 0;
    for (auto subseq_it = subseq.begin(); subseq_it != subseq.end();)
    {
        if (it == str.end())
            return {};
        const Codepoint c = utf8::read_codepoint(subseq_it, subseq.end());
        while (true)
        {
            auto str_c = utf8::read_codepoint(it, str.end());
            if (smartcase_eq(str_c, c))
                break;

            if (max_index != -1 && single_word &&  ! is_word(str_c))
                single_word = false;

            ++index;
            if (it == str.end())
                return {};
        }
        max_index = index++;
    }
    return SubseqRes{max_index, single_word};
}

template<typename TestFunc>
RankedMatch::RankedMatch(StringView candidate, StringView query, TestFunc func)
{
    if (candidate.empty() || query.length() > candidate.length())
        return;

    if (query.empty())
    {
        m_candidate = candidate;
        return;
    }

    if (! func())
        return;

    auto res = subsequence_match_smart_case(candidate, query);
    if (! res)
        return;

    m_candidate = candidate;
    m_max_index = res->max_index;

    if (res->single_word)
        m_flags |= Flags::SingleWord;
    if (smartcase_eq(candidate[0], query[0]))
        m_flags |= Flags::FirstCharMatch;

    auto it = std::search(candidate.begin(), candidate.end(),
                          query.begin(), query.end(), smartcase_eq);
    if (it != candidate.end())
    {
        m_flags |= Flags::Contiguous;
        if (it == candidate.begin())
        {
            m_flags |= Flags::Prefix;
            if (query.length() == candidate.length())
            {
                m_flags |= Flags::SmartFullMatch;
                if (candidate == query)
                    m_flags |= Flags::FullMatch;
            }
        }
    }

    m_word_boundary_match_count = count_word_boundaries_match(candidate, query);
    if (m_word_boundary_match_count == query.length())
        m_flags |= Flags::OnlyWordBoundary;
}

RankedMatch::RankedMatch(StringView candidate, UsedLetters candidate_letters,
                         StringView query, UsedLetters query_letters)
    : RankedMatch{candidate, query, [&] {
// Original code, doesn't parse and produces a MISSING node (semicolon ';'):
//        return matches(to_lower(query_letters), to_lower(candidate_letters)) and
//               matches(query_letters & upper_mask, candidate_letters & upper_mask);
        return matches(to_lower(query_letters), to_lower(candidate_letters));
    }} {}


RankedMatch::RankedMatch(StringView candidate, StringView query)
    : RankedMatch{candidate, query, [] { return true; }}
{
}

static bool is_word_boundary(Codepoint prev, Codepoint c)
{
    return (iswalnum((wchar_t)prev)) != iswalnum((wchar_t)c) ||
           (iswlower((wchar_t)prev) != islower((wchar_t)c));
}

bool RankedMatch::operator<(const RankedMatch& other) const
{
    kak_assert((bool)*this && (bool)other);

    const auto diff = m_flags ^ other.m_flags;
    // flags are different, use their ordering to return the first match
    if (diff != Flags::None)
        return (int)(m_flags & diff) > (int)(other.m_flags & diff);

    // If we are SingleWord, FirstCharMatch will do the job, and we dont want to take
    // other words boundaries into account.
    if (! (m_flags & (Flags::Prefix | Flags::SingleWord)) &&
        m_word_boundary_match_count != other.m_word_boundary_match_count)
        return m_word_boundary_match_count > other.m_word_boundary_match_count;

    if (m_max_index != other.m_max_index)
        return m_max_index < other.m_max_index;

    // Reorder codepoints to improve matching behaviour
    auto order = [](Codepoint cp) { return cp == '/' ? 0 : cp; };

    auto it1 = m_candidate.begin(), it2 = other.m_candidate.begin();
    const auto begin1 = it1, begin2 = it2;
    const auto end1 = m_candidate.end(), end2 = other.m_candidate.end();
    auto last1 = it1, last2 = it2;
    while (true)
    {
        // find next mismatch
        while (it1 != end1 && it2 != end2 && *it1 == *it2)
            ++it1, ++it2;

        if (it1 == end1 || it2 == end2)
            return it1 == end1 && it2 != end2;

        // compare codepoints
        it1 = utf8::character_start(it1, last1);
        it2 = utf8::character_start(it2, last2);
        const auto itsave1 = it1, itsave2 = it2;
        const auto cp1 = utf8::read_codepoint(it1, end1);
        const auto cp2 = utf8::read_codepoint(it2, end2);
        if (cp1 != cp2)
        {
            const auto cplast1 = utf8::prev_codepoint(itsave1, begin1).value_or(Codepoint{0});
            const auto cplast2 = utf8::prev_codepoint(itsave2, begin2).value_or(Codepoint{0});
            const bool is_wb1 = is_word_boundary(cplast1, cp1);
            const bool is_wb2 = is_word_boundary(cplast2, cp2);
            if (is_wb1 != is_wb2)
                return is_wb1;

            const bool low1 = iswlower((wchar_t)cp1);
            const bool low2 = iswlower((wchar_t)cp2);
            if (low1 != low2)
                return low1;

            return order(cp1) < order(cp2);
        }
        last1 = it1; last2 = it2;
    }
}

UnitTest test_ranked_match{[] {
    kak_assert(count_word_boundaries_match("run_all_tests", "rat") == 3);
    kak_assert(count_word_boundaries_match("run_all_tests", "at") == 2);
    kak_assert(count_word_boundaries_match("countWordBoundariesMatch", "wm") == 2);
    kak_assert(count_word_boundaries_match("countWordBoundariesMatch", "cobm") == 3);
    kak_assert(count_word_boundaries_match("countWordBoundariesMatch", "cWBM") == 4);
    kak_assert(RankedMatch{"source", "so"} < RankedMatch{"source_data", "so"});
    kak_assert(! (RankedMatch{"source_data", "so"} < RankedMatch{"source", "so"}));
    kak_assert(! (RankedMatch{"source", "so"} < RankedMatch{"source", "so"}));
    kak_assert(RankedMatch{"single/word", "wo"} < RankedMatch{"multiw/ord", "wo"});
    kak_assert(RankedMatch{"foo/bar/foobar", "foobar"} < RankedMatch{"foo/bar/baz", "foobar"});
    kak_assert(RankedMatch{"delete-buffer", "db"} < RankedMatch{"debug", "db"});
    kak_assert(RankedMatch{"create_task", "ct"} < RankedMatch{"constructor", "ct"});
    kak_assert(RankedMatch{"class", "cla"} < RankedMatch{"class::attr", "cla"});
    kak_assert(RankedMatch{"meta/", "meta"} < RankedMatch{"meta-a/", "meta"});
    kak_assert(RankedMatch{"find(1p)", "find"} < RankedMatch{"findfs(8)", "find"});
    kak_assert(RankedMatch{"find(1p)", "fin"} < RankedMatch{"findfs(8)", "fin"});
    kak_assert(RankedMatch{"sys_find(1p)", "sys_find"} < RankedMatch{"sys_findfs(8)", "sys_find"});
    kak_assert(RankedMatch{"init", ""} < RankedMatch{"__init__", ""});
    kak_assert(RankedMatch{"init", "ini"} < RankedMatch{"__init__", "ini"});
    kak_assert(RankedMatch{"a", ""} < RankedMatch{"b", ""});
    kak_assert(RankedMatch{"expresions", "expresins"} < RankedMatch{"expressionism's", "expresins"});
}};

UnitTest test_used_letters{[]()
{
    kak_assert(used_letters("abcd") == to_lower(used_letters("abcdABCD")));
}};

}

/*
  The 'and' operator exists in C++ as an alternative to '&&'.
  Same for 'or' ('||') and 'not' ('!').

  Copied from https://github.com/mawww/kakoune
  Public Domain.
*/

namespace Kakoune {

  UsedLetters used_letters(StringView str)
  {
    UsedLetters res = 0;
    for (auto c : str)
      {
        if (c >= 'a' and c <= 'z')
          res |= 1uLL << (c - 'a');
        else if (c >= 'A' and c <= 'Z')
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

}

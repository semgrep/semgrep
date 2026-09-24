//ERROR: match
namespace [[gnu::visibility("default")]] visible {
int first;
}

namespace hidden {
int second;
}

namespace [[gnu::visibility("hidden")]] also_hidden {
int third;
}

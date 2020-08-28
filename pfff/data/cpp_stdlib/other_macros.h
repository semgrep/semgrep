// TODO: now you can have a pfff_macros.h local per-project file (like I have
// a local per-projet skip list), so distribute this to the relevant
// places in software-src/

// ****************************************************************************
// git stuff
// ****************************************************************************

#define internal_function
#define NOINLINE
#define FORCEINLINE
#define THROWSPEC
#define NORETURN
#define NORETURN_PTR
#define MAYBE_UNUSED

#define __MINGW_NOTHROW
#define WSAAPI

#define for_each_string_list_item(a,b) for(;;)

// ****************************************************************************
// sparse stuff
// ****************************************************************************

//YACFE_MACROITERATOR
#define FOR_EACH_PTR(a,b) for(;;)
#define FOR_EACH_PTR_NOTAG(a,b) for(;;)
#define FOR_EACH_PTR_REVERSE(a,b) for(;;)
#define RECURSE_PTR_REVERSE(a,b) for(;;)
//#define DO_END_FOR_EACH_REVERSE(a,b,c,d)

#define __percpu
#define __user

#define SENTINEL_ATTR
#define FORMAT_ATTR(a)
#define NORETURN_ATTR

// ****************************************************************************
// Boost
// ****************************************************************************
#define BOOST_AUTO_TEST_CASE(a) void a()
#define BOOST_STATIC_ASSERT(a) static int x
#define BOOST_FOR_EACH(a,b) for(;;)
#define BOOST_FOREACH(a,b) for(;;)

// ****************************************************************************
// KDE
// ****************************************************************************
#define Q_OBJECT
#define Q_CLASSINFO(a,b)

// ****************************************************************************
// Jansson
// ****************************************************************************

#define JSON_INLINE
#define json_object_foreach(a,b,c) for(;;)

// ****************************************************************************
// Plan9
// ****************************************************************************

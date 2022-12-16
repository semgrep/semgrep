<?php

// ex of IDL data in the HipHop source


///////////////////////////////////////////////////////////////////////////////
// Constants
//
// array (
//   'name' => name of the constant
//   'type' => type of the constant
//   'note' => additional note about this constant's schema
// )

DefineConstant(
  array(
    'name'   => "INF",
    'type'   => Double,
  ));

///////////////////////////////////////////////////////////////////////////////
// Functions
//
// array (
//   'name'   => name of the function
//   'desc'   => description of the function's purpose
//   'flags'  => attributes of the function, see base.php for possible values
//   'opt'    => optimization callback function's name for compiler
//   'note'   => additional note about this function's schema
//   'return' =>
//      array (
//        'type'  => return type, use Reference for ref return
//        'desc'  => description of the return value
//      )
//   'args'   => arguments
//      array (
//        'name'  => name of the argument
//        'type'  => type of the argument, use Reference for output parameter
//        'value' => default value of the argument
//        'desc'  => description of the argument
//      )
// )

DefineFunction(
  array(
    'name'   => "connection_aborted",
    'desc'   => "Checks whether the client disconnected.",
    'flags'  =>  HasDocComment,
    'return' => array(
      'type'   => Int32,
      'desc'   => "Returns 1 if client disconnected, 0 otherwise.",
    ),
  ));


///////////////////////////////////////////////////////////////////////////////
// Classes
//
// BeginClass
// array (
//   'name'   => name of the class
//   'desc'   => description of the class's purpose
//   'flags'  => attributes of the class, see base.php for possible values
//   'note'   => additional note about this class's schema
//   'parent' => parent class name, if any
//   'ifaces' => array of interfaces tihs class implements
//   'bases'  => extra internal and special base classes this class requires
//   'footer' => extra C++ inserted at end of class declaration
// )
//
// DefineConstant(..)
// DefineConstant(..)
// ...
// DefineFunction(..)
// DefineFunction(..)
// ...
// DefineProperty
// DefineProperty
//
// array (
//   'name'  => name of the property
//   'type'  => type of the property
//   'flags' => attributes of the property
//   'desc'  => description of the property
//   'note'  => additional note about this property's schema
// )
//
// EndClass()


BeginClass(
  array(
    'name'   => "SQLite3",
    'desc'   => "A class that interfaces SQLite 3 databases.",
    'flags'  =>  HasDocComment,
    'footer' => <<<EOT

  public: void validate() const;
  public: sqlite3 *m_raw_db;
    DECLARE_BOOST_TYPES(UserDefinedFunc);
    struct UserDefinedFunc {
      int argc;
      Variant func;
      Variant step;
      Variant fini;
    };
  public: UserDefinedFuncPtrVec m_udfs;
EOT
,
  ));

DefineFunction(
  array(
    'name'   => "__construct",
    'flags'  =>  HasDocComment,
    'return' => array(
      'type'   => null,
    ),
  ));

EndClass(
);

<?php

// ------------------------------------------
// Misc
// ------------------------------------------

//from _checkModuleLib.php in $extension_defines
define('MCC_NZLIB_COMPRESSION', 0);
define('MCC_ARG_FB_SERIALIZE_PREFIXES', 0);

define('MCC_COMPRESSION_THRESHHOLD', 0);

// ------------------------------------------
// In hphp/facebook/extensions/ idl files
// ------------------------------------------

//TODO: autogenerate them too
function fbobj_hphp_register_config_func($str, $version) { }

function fbobj_hphp_create(int $fbtype, int $profile, int $fbid, array
                           $tao_response) { }

// ------------------------------------------
// used by third party code
// ------------------------------------------

define('_SYSTEM_TTFONTS', 0);

// defined ?? this flib/third-party/geojson/WKT/WKT.class.php defines
// only class GeoJSONWKT. Maybe some magic done around class name
class WKT {
  public static function load($x) { }
}

// ------------------------------------------
// Defined in some scripts
// ------------------------------------------
// scripts/memcache/sync_mcconf_to_smc.php
define('SCRIPT_IDENTIFIER', 0);
define('SCRIPT_OWNER_FBID', 0);

// ------------------------------------------
// Covered by some if(function_exists(...))
// ------------------------------------------

// we need to include it there because scheck is not
// aware of the if(function_exists(...) idion. todo?

function syck_load($xs) { }

// defined in flib/autoload/autoload_map.php but skipped by codegraph
function __flib_autoload_get_function_map() { }
function __flib_autoload_get_class_map() { }
function __flib_autoload_get_type_map() { }
function __flib_autoload_get_constant_map() { }

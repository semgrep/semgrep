<?hh
$tainted = Vector<int> {source()};
// ruleid: hack_collection_taint
sink($tainted);
$clean = Vector<int> {1, 2};
// ok: hack_collection_taint
sink($clean);

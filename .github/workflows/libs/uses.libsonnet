local lockfile = (import './uses.json').dependencies;

// Convert the lockfile structure of
// {
//  "org": {
//     "action-name": "version",
//     ...
//  },
//  ...
// }
// to a jsonnet structure of
// {
//   org: {
//     action_name: "org/action-name@version",
//     ...
//   },
//   ...
// }
// So when someone wants to use an action they can just write
// {
//   uses: uses.org.action_name
//   with: { ... }
// }

local prefix_action_versions(repo, action_versions) =
  std.mapWithKey(
    function(action_name, version)
      repo + '/' + action_name + '@' + version,
    action_versions,
  );

// action names can have dashes but then we won't be able to use the jsonnet dot
// access syntax. So normalize them by replacing dashes with underscores. so
// instead of writing uses.repo['action-name'] we can write
// uses.repo.action_name
local normalize_key(key) = std.strReplace(key, '-', '_');

local normalize_keys(obj) =
  std.foldl(
    function(acc, key)
      acc { [normalize_key(key)]: obj[key] },
    std.objectFields(obj),
    {},
  );

// actually map object
local uses = std.mapWithKey(function(repo, action_version) prefix_action_versions(repo, action_version), lockfile);

// normalize keys for dot access
std.foldl(
  function(acc, key)
    acc { [normalize_key(key)]: normalize_keys(uses[key]) },
  std.objectFields(uses),
  {},
)

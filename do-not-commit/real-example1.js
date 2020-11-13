// simpplified example from https://github.com/cthackers/adm-zip/pull/212/commits/6f4dfeb9a2166e93207443879988f97d88a37cde
var Utils = require("./util");

var fs = Utils.FileSystem.require(),
    pth = require("path");
fs.existsSync = fs.existsSync || pth.existsSync;
var isWin = /^win/.test(process.platform);


module.exports = function(/*String*/input) {

    return {
        extractEntryTo : function(/*Object*/entry, /*String*/targetPath, /*Boolean*/maintainEntryPath, /*Boolean*/overwrite) {
            overwrite = overwrite || false;
            maintainEntryPath = typeof maintainEntryPath == "undefined" ? true : maintainEntryPath;

            var item = getEntry(entry);
            if (!item) {
                throw Utils.Errors.NO_ENTRY;
            }

            var entryName = item.entryName;

            if(isWin){
                entryName = escapeFileName(entryName)
            }

            // ruleid:path-join-resolve-traversal
            var target = pth.resolve(targetPath, maintainEntryPath ? entryName : pth.basename(entryName));
            Utils.writeFileTo(target, content, overwrite);

            return true;
        }
    }
};

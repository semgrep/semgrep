let src = Logs.Src.create "semgrep.spacegrep"

module Log = (val Logs.src_log src : Logs.LOG)

let src = Logs.Src.create "semgrep.fixing"

module Log = (val Logs.src_log src : Logs.LOG)

let src = Logs.Src.create "semgrep.engine"

module Log = (val Logs.src_log src : Logs.LOG)

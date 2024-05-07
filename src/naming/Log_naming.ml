let src = Logs.Src.create "semgrep.naming"

module Log = (val Logs.src_log src : Logs.LOG)

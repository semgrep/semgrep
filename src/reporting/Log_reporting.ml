let src = Logs.Src.create "semgrep.reporting"

module Log = (val Logs.src_log src : Logs.LOG)

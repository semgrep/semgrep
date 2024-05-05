let src = Logs.Src.create "semgrep.core"

module Log = (val Logs.src_log src : Logs.LOG)

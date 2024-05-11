let src = Logs.Src.create "semgrep.printing"

module Log = (val Logs.src_log src : Logs.LOG)

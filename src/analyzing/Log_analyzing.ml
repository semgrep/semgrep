let src = Logs.Src.create "semgrep.analyzing"

module Log = (val Logs.src_log src : Logs.LOG)

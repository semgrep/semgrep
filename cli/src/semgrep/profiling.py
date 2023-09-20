# TODO: should just reuse the 'profile' type in semgrep_output_v1.atd
# and get rid of this whole file
import semgrep.semgrep_interfaces.semgrep_output_v1 as out


class ProfilingData:
    profile: out.CoreTiming

    def __init__(self, profile: out.CoreTiming) -> None:
        self.profile = profile

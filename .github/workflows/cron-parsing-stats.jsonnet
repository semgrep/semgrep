// Cron to update our parsing statistics which are then accessible at
// https://dashboard.semgrep.dev/metrics and also at
// https://metabase.corp.r2c.dev/collection/59-semgrep

// cron table memento below
// (src: https://dev.to/anshuman_bhardwaj/free-cron-jobs-with-github-actions-31d6)
// ┌────────── minute (0 - 59)
// │ ┌────────── hour (0 - 23)
// │ │ ┌────────── day of the month (1 - 31)
// │ │ │ ┌────────── month (1 - 12)
// │ │ │ │ ┌────────── day of the week (0 - 6)
// │ │ │ │ │
// │ │ │ │ │
// │ │ │ │ │
// * * * * *

local actions = import 'libs/actions.libsonnet';
local semgrep = import 'libs/semgrep.libsonnet';

// ----------------------------------------------------------------------------
// The job
// ----------------------------------------------------------------------------

local job = {
  'runs-on': 'ubuntu-latest',
  container: 'returntocorp/semgrep:develop',
  steps: [
    actions.checkout(),
    // The packages below used to be part of the returntocorp/semgrep docker
    // image but got removed to reduce its attack surface, so we need
    // to install them now
    {
      name: 'Install dependencies',
      run: 'apk add bash jq curl',
    },
    {
      // Run parsing stats and publish them to the semgrep dashboard.
      run: |||
            cd stats/parsing-stats
            ./run-all --upload
      |||,
    },
  ],
};
//          no_output_timeout: 60m
//      - store_artifacts:
//          path: stats/parsing-stats/logs
//      - store_artifacts:
//          path: stats/parsing-stats/results.txt

// ----------------------------------------------------------------------------
// The Workflow
// ----------------------------------------------------------------------------
{
  name: 'cron-parsing-stats',
  on: {
    // This allows to trigger manually the workflow! (this is one of the few
    // things where GHA is actually better than circleCI).
    workflow_dispatch: null,
    schedule: [
      {
        // every day at 7:26
        cron: '26 7 * * *',
      },
    ],
  },
  jobs: {
    job: job,
    //TODO: abusing the nightly notif because the other do not work
    'notify-failure': semgrep.slack.notify_failure_nightly_job(
        'Actually the cron parsing stats failed', 'https://github.com/semgrep/semgrep/actions/workflows/cron-parsing-stats.yml ') +
      { needs: ['job'] },
  },
}

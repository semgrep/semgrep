// https://github.com/returntocorp/semgrep-app/pull/5203/files

import { useHistory } from "react-router-dom";

const SetupProjectPanelCircleCI = () => {
  const basePath = useBasePath();
  const history = useHistory();

  return (
    <>
      <ScanOptionContainer>
        <HeaderWrapper>Add CI job to CircleCI pipeline</HeaderWrapper>
        <InstructionGuide>
          <ol>
            <li>
              Add your app token as a project{" "}
              <ExternalLink
                href="https://circleci.com/docs/2.0/env-vars/"
                stayVisible
              >
                environment variable
              </ExternalLink>{" "}
              named{" "}
              <Code inline light>
                SEMGREP_APP_TOKEN
              </Code>
              .
            </li>
            <li>
              Add the snippet below to your&nbsp;
              <Code inline light>
                .circleci/config.yml
              </Code>
              .
            </li>
          </ol>
          <Button
            large
            intent="primary"
            // ruleid: dlukeomalley.use-absolute-paths-for-routing
            onClick={() => history.push(`${basePath}/projects`)}
            style={{ marginRight: "8px" }}
          >
            View Added Project
          </Button>
        </InstructionGuide>
        <CircleCICode withSwitches />
      </ScanOptionContainer>
    </>
  );
};

export default SetupProjectPanelCircleCI;

// https://github.com/returntocorp/semgrep/issues/5115

const Sources: React.FC<{
  rule: Rule;
  references: string[] | undefined;
  license: string | undefined;
}> = ({ rule, references, license }) => {
  return (
    <div>
      {rule.visibility !== "org_private" && (
        <div>
          <SourceLink
            target="_blank"
            rel="noopener noreferrer"
            href={validateUrl(rule.source_uri)}
          >
            [0] Source for rule
          </SourceLink>
        </div>
      )}
      {references && (
        <>
          {references.map((url, index) => (
            <div>
              <SourceLink target="_blank" rel="noopener noreferrer" href={validateUrl(url)}>
                {`[${index + 1}] ${url}`}
              </SourceLink>
            </div>
          ))}
        </>
      )}
      <License>License: {license || "N/A - Check source repository"}</License>
    </div>
  );
};

const Sources: React.FC<{
  rule: Rule;
  references: string[] | undefined;
  license: string | undefined;
}> = ({ rule, references, license }) => {
  return (
    <div>
      {rule.visibility !== "org_private" && (
        <div>
          <SourceLink
            target="_blank"
            rel="noopener noreferrer"
            //ruleid: href-semgrep-app
            href={rule.source_uri}
          >
            [0] Source for rule
          </SourceLink>
        </div>
      )}
      {references && (
        <>
          {references.map((url, index) => (
            <div>
              {/* ruleid: href-semgrep-app */}
              <SourceLink target="_blank" rel="noopener noreferrer" href={url}>
                {`[${index + 1}] ${url}`}
              </SourceLink>
            </div>
          ))}
        </>
      )}
      <License>License: {license || "N/A - Check source repository"}</License>
    </div>
  );
};

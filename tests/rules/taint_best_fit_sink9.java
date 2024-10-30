package test.sqlInjection;

import java.util.Map;
import org.sasanlabs.vulnerability.utils.Constants;
import org.springframework.http.ResponseEntity;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.web.bind.annotation.RequestParam;

public class ErrorBasedSQLInjectionVulnerability {

    public ResponseEntity<String> doesCarInformationExistsLevel4(
            @RequestParam Map<String, String> queryParams) {
        final String id = queryParams.get(Constants.ID).replaceAll("'", "");
        ResponseEntity<String> response =
                applicationJdbcTemplate.query(
                        (conn) ->
                                // Want "best fit matches" to be computed once per
                                // function definition and then used as-is for the
                                // analysis of the lambdas. If we computed them
                                // separately for lambdas, then this statement would
                                // be a best match. In this particular case it may
                                // be convenient, but it's better to rely on proper
                                // inter-proc analysis.
                                conn.prepareStatement(
                                        "select * from cars where id='" + id + "'"));

        return response;
    }

}

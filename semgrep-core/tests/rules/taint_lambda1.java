package org.sasanlabs.service.vulnerability.sqlInjection;

import com.fasterxml.jackson.core.JsonProcessingException;
import java.util.Map;
import java.util.function.Function;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.sasanlabs.internal.utility.JSONSerializationUtils;
import org.sasanlabs.internal.utility.LevelConstants;
import org.sasanlabs.internal.utility.Variant;
import org.sasanlabs.internal.utility.annotations.AttackVector;
import org.sasanlabs.internal.utility.annotations.VulnerableAppRequestMapping;
import org.sasanlabs.internal.utility.annotations.VulnerableAppRestController;
import org.sasanlabs.vulnerability.types.VulnerabilityType;
import org.sasanlabs.vulnerability.utils.Constants;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.ResponseEntity.BodyBuilder;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.web.bind.annotation.RequestParam;

/**
 * Error Based SQLInjection is the easiest way for extracting data and a very dangerous way which
 * can lead to serious impacts and can compromise the entire system.
 *
 * @author preetkaran20@gmail.com KSASAN
 */
@VulnerableAppRestController(
        descriptionLabel = "SQL_INJECTION_VULNERABILITY",
        value = "ErrorBasedSQLInjectionVulnerability")
public class ErrorBasedSQLInjectionVulnerability {

    private JdbcTemplate applicationJdbcTemplate;

    private static final transient Logger LOGGER =
            LogManager.getLogger(ErrorBasedSQLInjectionVulnerability.class);

    private static final Function<Exception, String> GENERIC_EXCEPTION_RESPONSE_FUNCTION =
            (ex) -> "{ \"isCarPresent\": false, \"moreInfo\": " + ex.getMessage() + "}";
    static final String CAR_IS_NOT_PRESENT_RESPONSE = "{ \"isCarPresent\": false}";
    static final Function<String, String> CAR_IS_PRESENT_RESPONSE =
            (carInformation) ->
                    "{ \"isCarPresent\": true, \"carInformation\":" + carInformation + "}";

    public ErrorBasedSQLInjectionVulnerability(
            @Qualifier("applicationJdbcTemplate") JdbcTemplate applicationJdbcTemplate) {
        this.applicationJdbcTemplate = applicationJdbcTemplate;
    }

    @AttackVector(
            vulnerabilityExposed = VulnerabilityType.ERROR_BASED_SQL_INJECTION,
            description = "ERROR_SQL_INJECTION_URL_PARAM_APPENDED_DIRECTLY_TO_QUERY",
            payload = "ERROR_BASED_SQL_INJECTION_PAYLOAD_LEVEL_1")
    @VulnerableAppRequestMapping(
            value = LevelConstants.LEVEL_1,
            htmlTemplate = "LEVEL_1/SQLInjection_Level1")
    public ResponseEntity<String> doesCarInformationExistsLevel1(
            @RequestParam Map<String, String> queryParams) {
        String id = queryParams.get(Constants.ID);
        BodyBuilder bodyBuilder = ResponseEntity.status(HttpStatus.OK);
        try {
            ResponseEntity<String> response =
                    applicationJdbcTemplate.query(
                            // ruleid: jdbc
                            "select * from cars where id=" + id,
                            (rs) -> {
                                if (rs.next()) {
                                    CarInformation carInformation = new CarInformation();
                                    carInformation.setId(rs.getInt(1));
                                    carInformation.setName(rs.getString(2));
                                    carInformation.setImagePath(rs.getString(3));
                                    try {
                                        return bodyBuilder.body(
                                                CAR_IS_PRESENT_RESPONSE.apply(
                                                        JSONSerializationUtils.serialize(
                                                                carInformation)));
                                    } catch (JsonProcessingException e) {
                                        LOGGER.error("Following error occurred", e);
                                        return bodyBuilder.body(
                                                GENERIC_EXCEPTION_RESPONSE_FUNCTION.apply(e));
                                    }
                                } else {
                                    return bodyBuilder.body(
                                            ErrorBasedSQLInjectionVulnerability
                                                    .CAR_IS_NOT_PRESENT_RESPONSE);
                                }
                            });
            return response;
        } catch (Exception ex) {
            LOGGER.error("Following error occurred", ex);
            return bodyBuilder.body(GENERIC_EXCEPTION_RESPONSE_FUNCTION.apply(ex));
        }
    }

    @AttackVector(
            vulnerabilityExposed = VulnerabilityType.ERROR_BASED_SQL_INJECTION,
            description =
                    "ERROR_SQL_INJECTION_URL_PARAM_WRAPPED_WITH_SINGLE_QUOTE_APPENDED_TO_QUERY",
            payload = "ERROR_BASED_SQL_INJECTION_PAYLOAD_LEVEL_2")
    @VulnerableAppRequestMapping(
            value = LevelConstants.LEVEL_2,
            htmlTemplate = "LEVEL_1/SQLInjection_Level1")
    public ResponseEntity<String> doesCarInformationExistsLevel2(
            @RequestParam Map<String, String> queryParams) {
        String id = queryParams.get(Constants.ID);
        BodyBuilder bodyBuilder = ResponseEntity.status(HttpStatus.OK);
        try {
            ResponseEntity<String> response =
                    applicationJdbcTemplate.query(
                            // ruleid: jdbc
                            "select * from cars where id='" + id + "'",
                            (rs) -> {
                                if (rs.next()) {
                                    CarInformation carInformation = new CarInformation();
                                    carInformation.setId(rs.getInt(1));
                                    carInformation.setName(rs.getString(2));
                                    carInformation.setImagePath(rs.getString(3));
                                    try {
                                        return bodyBuilder.body(
                                                CAR_IS_PRESENT_RESPONSE.apply(
                                                        JSONSerializationUtils.serialize(
                                                                carInformation)));
                                    } catch (JsonProcessingException e) {
                                        LOGGER.error("Following error occurred", e);
                                        return bodyBuilder.body(
                                                GENERIC_EXCEPTION_RESPONSE_FUNCTION.apply(e));
                                    }
                                } else {
                                    return bodyBuilder.body(
                                            ErrorBasedSQLInjectionVulnerability
                                                    .CAR_IS_NOT_PRESENT_RESPONSE);
                                }
                            });
            return response;
        } catch (Exception ex) {
            LOGGER.error("Following error occurred", ex);
            return bodyBuilder.body(GENERIC_EXCEPTION_RESPONSE_FUNCTION.apply(ex));
        }
    }

    // https://stackoverflow.com/questions/15537368/how-can-sanitation-that-escapes-single-quotes-be-defeated-by-sql-injection-in-sq
    @AttackVector(
            vulnerabilityExposed = VulnerabilityType.ERROR_BASED_SQL_INJECTION,
            description =
                    "ERROR_SQL_INJECTION_URL_PARAM_REMOVES_SINGLE_QUOTE_WRAPPED_WITH_SINGLE_QUOTE_APPENDED_TO_QUERY",
            payload = "ERROR_BASED_SQL_INJECTION_PAYLOAD_LEVEL_3")
    @VulnerableAppRequestMapping(
            value = LevelConstants.LEVEL_3,
            htmlTemplate = "LEVEL_1/SQLInjection_Level1")
    public ResponseEntity<String> doesCarInformationExistsLevel3(
            @RequestParam Map<String, String> queryParams) {
        String id = queryParams.get(Constants.ID);
        id = id.replaceAll("'", "");
        BodyBuilder bodyBuilder = ResponseEntity.status(HttpStatus.OK);
        bodyBuilder.body(ErrorBasedSQLInjectionVulnerability.CAR_IS_NOT_PRESENT_RESPONSE);
        try {
            ResponseEntity<String> response =
                    applicationJdbcTemplate.query(
                            // ruleid: jdbc
                            "select * from cars where id='" + id + "'",
                            (rs) -> {
                                if (rs.next()) {
                                    CarInformation carInformation = new CarInformation();

                                    carInformation.setId(rs.getInt(1));
                                    carInformation.setName(rs.getString(2));
                                    carInformation.setImagePath(rs.getString(3));
                                    try {
                                        return bodyBuilder.body(
                                                CAR_IS_PRESENT_RESPONSE.apply(
                                                        JSONSerializationUtils.serialize(
                                                                carInformation)));
                                    } catch (JsonProcessingException e) {
                                        LOGGER.error("Following error occurred", e);
                                        return bodyBuilder.body(
                                                GENERIC_EXCEPTION_RESPONSE_FUNCTION.apply(e));
                                    }
                                } else {
                                    return bodyBuilder.body(
                                            ErrorBasedSQLInjectionVulnerability
                                                    .CAR_IS_NOT_PRESENT_RESPONSE);
                                }
                            });

            return response;
        } catch (Exception ex) {
            LOGGER.error("Following error occurred", ex);
            return bodyBuilder.body(GENERIC_EXCEPTION_RESPONSE_FUNCTION.apply(ex));
        }
    }

    // Assumption that only creating PreparedStatement object can save is wrong. You
    // need to use the parameterized query properly.
    @AttackVector(
            vulnerabilityExposed = VulnerabilityType.ERROR_BASED_SQL_INJECTION,
            description = "ERROR_SQL_INJECTION_URL_PARAM_APPENDED_TO_PARAMETERIZED_QUERY",
            payload = "ERROR_BASED_SQL_INJECTION_PAYLOAD_LEVEL_4")
    @VulnerableAppRequestMapping(
            value = LevelConstants.LEVEL_4,
            htmlTemplate = "LEVEL_1/SQLInjection_Level1")
    public ResponseEntity<String> doesCarInformationExistsLevel4(
            @RequestParam Map<String, String> queryParams) {
        final String id = queryParams.get(Constants.ID).replaceAll("'", "");
        BodyBuilder bodyBuilder = ResponseEntity.status(HttpStatus.OK);
        bodyBuilder.body(ErrorBasedSQLInjectionVulnerability.CAR_IS_NOT_PRESENT_RESPONSE);
        try {
            ResponseEntity<String> response =
                    applicationJdbcTemplate.query(
                            (conn) ->
                                    conn.prepareStatement(
                                            // ruleid: jdbc
                                            "select * from cars where id='" + id + "'"),
                            (ps) -> {},
                            (rs) -> {
                                if (rs.next()) {
                                    CarInformation carInformation = new CarInformation();

                                    carInformation.setId(rs.getInt(1));
                                    carInformation.setName(rs.getString(2));
                                    carInformation.setImagePath(rs.getString(3));
                                    try {
                                        return bodyBuilder.body(
                                                CAR_IS_PRESENT_RESPONSE.apply(
                                                        JSONSerializationUtils.serialize(
                                                                carInformation)));
                                    } catch (JsonProcessingException e) {
                                        LOGGER.error("Following error occurred", e);
                                        return bodyBuilder.body(
                                                GENERIC_EXCEPTION_RESPONSE_FUNCTION.apply(e));
                                    }
                                } else {
                                    return bodyBuilder.body(
                                            ErrorBasedSQLInjectionVulnerability
                                                    .CAR_IS_NOT_PRESENT_RESPONSE);
                                }
                            });

            return response;
        } catch (Exception ex) {
            LOGGER.error("Following error occurred", ex);
            return bodyBuilder.body(GENERIC_EXCEPTION_RESPONSE_FUNCTION.apply(ex));
        }
    }

    @VulnerableAppRequestMapping(
            value = LevelConstants.LEVEL_5,
            variant = Variant.SECURE,
            htmlTemplate = "LEVEL_1/SQLInjection_Level1")
    public ResponseEntity<String> doesCarInformationExistsLevel5(
            @RequestParam Map<String, String> queryParams) {
        final String id = queryParams.get(Constants.ID);
        BodyBuilder bodyBuilder = ResponseEntity.status(HttpStatus.OK);
        bodyBuilder.body(ErrorBasedSQLInjectionVulnerability.CAR_IS_NOT_PRESENT_RESPONSE);
        try {
            ResponseEntity<String> responseEntity =
                    applicationJdbcTemplate.query(
                            (conn) -> conn.prepareStatement("select * from cars where id=?"),
                            // ok: jdbc
                            (prepareStatement) -> {
                                prepareStatement.setString(1, id);
                            },
                            (rs) -> {
                                CarInformation carInformation = new CarInformation();
                                if (rs.next()) {
                                    carInformation.setId(rs.getInt(1));
                                    carInformation.setName(rs.getString(2));
                                    carInformation.setImagePath(rs.getString(3));

                                    try {
                                        return bodyBuilder.body(
                                                CAR_IS_PRESENT_RESPONSE.apply(
                                                        JSONSerializationUtils.serialize(
                                                                carInformation)));
                                    } catch (JsonProcessingException e) {
                                        LOGGER.error("Following error occurred", e);
                                        return bodyBuilder.body(
                                                ErrorBasedSQLInjectionVulnerability
                                                        .CAR_IS_NOT_PRESENT_RESPONSE);
                                    }
                                } else {
                                    return bodyBuilder.body(
                                            ErrorBasedSQLInjectionVulnerability
                                                    .CAR_IS_NOT_PRESENT_RESPONSE);
                                }
                            });

            return responseEntity;

        } catch (Exception ex) {
            LOGGER.error("Following error occurred", ex);
            return bodyBuilder.body(
                    ErrorBasedSQLInjectionVulnerability.CAR_IS_NOT_PRESENT_RESPONSE);
        }
    }
}

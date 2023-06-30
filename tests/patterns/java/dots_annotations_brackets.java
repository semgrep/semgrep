import org.springframework.http.HttpStatus;

@RestController
@RequestMapping("/api/v1/customers")
public class CustomerRestControllerImpl implements CustomerRestController {
    // ERROR: 
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "The customer has been successfully added."),
            @ApiResponse(responseCode = "404", description = "The passed customer details are not found"),
            @ApiResponse(responseCode = "409", description = "The passed customer id already exists"),
            @ApiResponse(responseCode = "500", description = "An error occurred during processing")
    })
    @PostMapping
    public CustomerDTO addCustomer(CustomerDTO customerDTO) {
        int test = 0;
        try {
            return this.customerService.addCustomer(customerDTO);
        } catch (CustomerAlreadyExistsException exists) {
            throw new ResponseStatusException(HttpStatus.CONFLICT, exists.getMessage());
        } catch (ConstraintViolationException constraintViolationException) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, constraintViolationException.getMessage());
        }
    }
}

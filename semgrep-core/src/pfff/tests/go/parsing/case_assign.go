package foo

func getExitStatus(errC <-chan error, resultC <-chan container.ContainerWaitOKBody) error {
	select {
	case result := <-resultC:
		if result.Error != nil {
			return fmt.Errorf(result.Error.Message)
		}
		if result.StatusCode != 0 {
			return cli.StatusError{StatusCode: int(result.StatusCode)}
		}
	case err := <-errC:
		return err
	}

	return nil
}

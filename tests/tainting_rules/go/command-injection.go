func (a *App) ServeHTTP(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/plain")
		w.WriteHeader(http.StatusOK)

  	username, _, _ := r.BasicAuth()

  	fmt.Printf("Path Sink:\n")
  	cmd := &exec.Cmd {
  	  // Path is the path of the command to run.
  	  // ruleid: command-injection
  	  Path: username,
  	  // Args holds command line arguments, including the command as Args[0].
  	  Args: []string{ "tr", "--help" },
  	  Stdout: os.Stdout,
  	  Stderr: os.Stderr,
  	}

  	fmt.Printf("Args Sink:\n")
  	cmd2 := &exec.Cmd {
  	  // Path is the path of the command to run.
  	  Path: "/usr/bin/tr",
  	  // Args holds command line arguments, including the command as Args[0].
  	  // ruleid: command-injection
  	  Args: []string{ username, "--help" },
  	  Stdout: os.Stdout,
  	  Stderr: os.Stderr,
  	}

  	cmd3 := &exec.Cmd {
  	  // Path is the path of the command to run.
  	  Path: "/usr/bin/tr",
  	  // Args holds command line arguments, including the command as Args[0].
  	  Args: []string{ "--help" },
  	  // ok: net-http-command-injection-taint
  	  Something: username,
  	  Stdout: os.Stdout,
  	  Stderr: os.Stderr,
  	}

  	// ruleid: command-injection
  	syscall.Exec(username, []string{"-a", "-l", "-h"})

  	// ok: net-http-command-injection-taint
  	syscall.Exec("ls", []string{"-a", "-l", "-h"}, username)
}


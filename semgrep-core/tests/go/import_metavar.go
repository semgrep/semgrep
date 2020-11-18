package foo

//ERROR: match
import log "github.com/sirupsen/logrus"

func main() {
  var err error
  log.WithError(err).Info("something")
}

package foo

import log "github.com/sirupsen/logrus"

func main2() {
  var log error
  log.WithError(err).Info("something")
}

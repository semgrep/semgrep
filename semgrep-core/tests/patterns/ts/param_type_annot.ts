// https://github.com/returntocorp/semgrep/issues/5004

module.exports = function servePublicFiles () {
  //MATCH:
  return ({ params }: Request, res: Response, next: NextFunction) => {
    const file = params.file
      res.sendFile(path.resolve('ftp/', params.file))
  }
}


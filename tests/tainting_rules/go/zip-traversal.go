import (
	"archive/zip"
	"fmt"
	"io"
	"log"
	"os"
	"path/filepath"
)

func main() {
	safe_unzip("/tmp/some-dir")
	dangerous_unzip("/tmp/some-dir")
}

func safe_unzip(target string) error {
	reader, err := zip.OpenReader("example.zip")
	if err := os.MkdirAll(target, 0750); err != nil {
		return err
	}
	for _, file := range reader.File {
		path := filepath.Join(target, file.Name)

		if !strings.HasPrefix(path, filepath.Clean(target) + string(os.PathSeparator)){
		  return filenames, fmt.Errorf("%s is an illegal filepath", path)
		}

		if file.FileInfo().IsDir() {
			os.MkdirAll(path, file.Mode())
			continue
		}
		fileReader, err := file.Open()
		defer fileReader.Close()
		//OK:
		targetFile, err := os.OpenFile(path, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, file.Mode())
		if err != nil {
			return err
		}
		defer targetFile.Close()
		if _, err := io.Copy(targetFile, fileReader); err != nil {
			return err
		}
	}
	return nil
}

func dangerous_unzip(target string) error {
	reader, err := zip.OpenReader("example.zip")
	if err := os.MkdirAll(target, 0750); err != nil {
		return err
	}
	for _, file := range reader.File {
		path := filepath.Join(target, file.Name)

		// No verification / sanitization this can "slip"

		if file.FileInfo().IsDir() {
			os.MkdirAll(path, file.Mode())
			continue
		}
		fileReader, err := file.Open()
		defer fileReader.Close()
		//ruleid: tainting
		targetFile, err := os.OpenFile(path, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, file.Mode())
		if err != nil {
			return err
		}
		defer targetFile.Close()
		if _, err := io.Copy(targetFile, fileReader); err != nil {
			return err
		}
	}
	return nil
}

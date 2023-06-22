package server

import (
	"os"
	"path/filepath"
	"strings"
)

func getAllPossibleFiles(fileName string) []string {
	parts := strings.Split(fileName, ".")
	ext := "." + parts[len(parts)-1]
	combinations := []string{fileName}

	for i := len(parts) - 2; i >= 1; i-- {
		combinations = append(combinations, strings.Join(parts[:i], ".")+ext)
	}

	return combinations
}

func getFirstMatchingFile(directory string, fileNames []string) string {
	for _, fileName := range fileNames {
		var matches []string
		err := filepath.Walk(directory, func(path string, info os.FileInfo, err error) error {
			if err != nil {
				return err
			}
			if !info.IsDir() {
				if fileName == info.Name() {
					matches = append(matches, path)
				}
			}
			return nil
		})

		if err != nil {
			continue
		}

		if matches == nil {
			continue
		}

		return matches[0]
	}

	return ""
}

class Test {
    public static void main(String[] args) {
        try (
        java.util.zip.ZipFile zf =
            // ERROR:
             new java.util.zip.ZipFile(zipFileName);
        java.io.BufferedWriter writer = 
            // ERROR: 
            java.nio.file.Files.newBufferedWriter(zipFileName, charset)
    ) {
        // Enumerate each entry
        for (java.util.Enumeration entries =
                                zf.entries(); entries.hasMoreElements();) {
            // Get the entry name and write it to the output file
            String newLine = System.getProperty("line.separator");
            String zipEntryName =
                 ((java.util.zip.ZipEntry)entries.nextElement()).getName() +
                 newLine;
            writer.write(zipEntryName, 0, zipEntryName.length());
        }
    }
    }
}

package testcode.file.permissions;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.HashSet;
import java.util.Set;

public class FileApi {

    public static void notOk() throws IOException {
        // ruleid:overly-permissive-file-permission
        Files.setPosixFilePermissions(Paths.get("/var/opt/app/init_script.sh"), PosixFilePermissions.fromString("rw-rw-rw-"));
        // ruleid:overly-permissive-file-permission
        Files.setPosixFilePermissions(Paths.get("/var/opt/configuration.xml"), PosixFilePermissions.fromString("rw-rw-r--"));
    }

    public static void notOk2() throws IOException {
        Set<PosixFilePermission> perms = new HashSet<>();
        perms.add(PosixFilePermission.OWNER_READ);
        perms.add(PosixFilePermission.OWNER_WRITE);
        perms.add(PosixFilePermission.OWNER_EXECUTE);

        perms.add(PosixFilePermission.GROUP_READ);
        perms.add(PosixFilePermission.GROUP_WRITE);
        perms.add(PosixFilePermission.GROUP_EXECUTE);

        // ruleid:overly-permissive-file-permission
        perms.add(PosixFilePermission.OTHERS_READ);
        // ruleid:overly-permissive-file-permission
        perms.add(PosixFilePermission.OTHERS_WRITE);
        // ruleid:overly-permissive-file-permission
        perms.add(PosixFilePermission.OTHERS_EXECUTE);

        Files.setPosixFilePermissions(Paths.get("/var/opt/app/init_script.sh"),perms);
    }

    public static void ok() throws IOException {
        Files.setPosixFilePermissions(Paths.get("/var/opt/configuration.xml"), PosixFilePermissions.fromString("rw-rw----"));
        Files.setPosixFilePermissions(Paths.get("/var/opt/configuration.xml"), PosixFilePermissions.fromString("rwxrwx---"));
    }
}

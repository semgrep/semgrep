package deserialize;

import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.IOException;
import java.lang.ClassNotFoundException;

import com.biz.org.UserData;

public class Cls
{
	public UserData deserializeObject(InputStream receivedFile) throws IOException, ClassNotFoundException {
        // ruleid:object-deserialization
        ObjectInputStream in = new ObjectInputStream(receivedFile);
        return (UserData) in.readObject();
    }

    public UserData deserializeObject(InputStream receivedFile) throws IOException, ClassNotFoundException {
	// this pattern not yet working. See https://github.com/returntocorp/semgrep/issues/717
	// This should have a To Do comment, but I want this rule available so I'm leaving it out for now.
        try (ObjectInputStream in = new ObjectInputStream(receivedFile)) {
            return (UserData) in.readObject();
        } catch (IOException e) {
            throw e;
        }
    }
}

public class Cls
{
//	public UserData deserializeObject(InputStream receivedFile) throws IOException, ClassNotFoundException {
//        // ruleid:object-deserialization
//        ObjectInputStream in = new ObjectInputStream(receivedFile);
//        return (UserData) in.readObject();
//    }

	public <T extends UserData> T deserializeObjectStealth(InputStream receivedFile) throws IOException, ClassNotFoundException {
        // ruleid:object-deserialization
        ObjectInputStream in = new ObjectInputStream(receivedFile);
        return (UserData) in.readObject();
    }
}

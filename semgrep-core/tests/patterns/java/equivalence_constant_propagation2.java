public class SemgrepTest
{
	public static final String MD5_1 = "MD5";
	public final String MD5_2 = "MD5";

public static void main(String[] args) throws NoSuchAlgorithmException, IOException
	{

		if (args.length != 1)
		{
			throw new IOException("Wrong number of arguments");
		}

        //ERROR: match
		MessageDigest md1 = MessageDigest.getInstance("MD5");

        //ERROR: match
		MessageDigest md2 = MessageDigest.getInstance(MD5_1);

        //ERROR: match
		MessageDigest md3 = MessageDigest.getInstance(MD5_2);	

		int stam1 = 0;
		if (stam1 == stam1)
			throw new IOException("Bad place");

	}

}
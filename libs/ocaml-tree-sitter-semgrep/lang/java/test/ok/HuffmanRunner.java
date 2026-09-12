/*
  Copied from
    https://github.com/kurtisnelson/Java-Huffman/blob/master/src/HuffmanRunner.java
  Public Domain.
*/

import java.util.ArrayList;
import java.util.List;

import com.kelsonprime.huffman.Encoding;
import com.kelsonprime.huffman.EncodingMap;
import com.kelsonprime.huffman.HuffmanEncoder;

public class HuffmanRunner {

	/**
	 * Example use of the <code>HuffmanEncoder</code>
	 * @url https://github.com/kurtisnelson/Java-Huffman
	 * @author Kurt Nelson
	 * @param args
	 */
	public static void main(String[] args) {
		String s = "This is a test, only a test. Please continue about your DAY!";
		EncodingMap<Character> map = new EncodingMap<Character>(
				toCharacterList(s));
		HuffmanEncoder<Character> he = new HuffmanEncoder<Character>(map);
		String encoded = Encoding.toString(he.encode(toCharacterList(s)));
		System.out.println("Encoded: " + encoded);
		List<Character> decoded = he.decode(encoded);
		System.out.println("Decoded: " + charactersToString(decoded));
	}

	/**
	 * Helper method to get data to encode into proper format.
	 * @param s String to convert
	 * @return List for use in Encoder
	 */
	public static List<Character> toCharacterList(String s) {
		List<Character> ret = new ArrayList<Character>(s.length());
		for (char c : s.toCharArray()) {
			ret.add(c);
		}
		return ret;
	}

	/**
	 * Helper method to convert returned <code>Character</code> arry into a string
	 * @param chars Array to convert
	 * @return converted String
	 */
	public static String charactersToString(List<Character> chars) {
		StringBuilder sb = new StringBuilder();
		for (char c : chars)
			sb.append(c);
		return sb.toString();
	}

}

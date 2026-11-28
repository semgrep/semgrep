// C# 12: collection expressions and the spread element `..`.
// https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/operators/collection-expressions

using System.Collections.Generic;

public class CollectionExprs
{
    public static void Demo()
    {
        // Empty collection literal targets List<int>.
        List<int> empty = [];

        // Inline literal.
        int[] xs = [1, 2, 3];

        // Mix literal with spreads.
        int[] head = [0];
        int[] tail = [4, 5];
        int[] all = [.. head, .. xs, .. tail];

        // Span<T> target.
        System.Span<int> span = [10, 20, 30];

        // Nested collection literal.
        int[][] grid = [
            [1, 2, 3],
            [4, 5, 6],
            [7, 8, 9],
        ];
    }
}

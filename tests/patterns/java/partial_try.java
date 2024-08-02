package org.apache.cassandra.index;

public class TargetParser
{
    public static Pair<ColumnMetadata, IndexTarget.Type> parse(TableMetadata metadata, String target)
    {
        // if the regex matches then the target is in the form "keys(foo)", "entries(bar)" etc
        // if not, then it must be a simple column name and implicitly its type is VALUES
        Matcher matcher = TARGET_REGEX.matcher(target);
        String columnName;
        IndexTarget.Type targetType;
        if (matcher.matches())
        {
            targetType = IndexTarget.Type.fromString(matcher.group(1));
            columnName = matcher.group(2);
        }
        else
        {
            columnName = target;
            targetType = IndexTarget.Type.VALUES;
        }

	//ERROR: match
        try {
          int i = 0;
        } catch (Exception e) {
          // comment
          log.info("Error: ", e);
        } catch (RuntimeException e) {
          log.info("Error: ", e);
          throw SneakyThrow.sneak(e);
        } 
        catch (IOException e) {
        }
    }
}

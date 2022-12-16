class X {

    public ApiRequest getRequest(Void aVoid) throws Exception {

      return new ApiRequest(
          "test",
          "GET",
          "test",
          ImmutableList.<NameValuePair>of(),
          type);

     UberbarResult expectedFriend2 = UberbarResults.createFriend(
         "image2",
         "Friend2",
         2,
         Lists.<String>newArrayList(null, null));

    }


}

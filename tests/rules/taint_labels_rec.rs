async fn test(client: Client, id: String) -> Option<Json<Post>> {
    // `id` has label INPUT and also FORMAT at the same time, but
    // FORMAT requires INPUT. So first we need to taint it with
    // INPUT, and then we need to check again for more labels, and
    // we will realized FORMAT also applies to it.
    // ruleid: test
    client.post(format!("https://{}", id));
}

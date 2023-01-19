fun main(args: Array<String>) {
    (URL(urlPath).openConnection() as HttpsURLConnection).apply {
	    //ERROR: match
            hostnameVerifier = HostnameVerifier { _, _ -> true }
        }.inputStream.use {
            it.copyTo(System.out)
        }
}

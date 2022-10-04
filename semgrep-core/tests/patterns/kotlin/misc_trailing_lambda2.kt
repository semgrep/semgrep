private fun createSocketFactory(protocols: List<String>) =
    SSLContext.getInstance(protocols[0]).apply {
        val trustAllCerts = arrayOf<TrustManager>(object : X509TrustManager {
	    //ERROR: match					      
            override fun getAcceptedIssuers(): Array<X509Certificate> = arrayOf()
            override fun checkClientTrusted(certs: Array<X509Certificate>, authType: String) = Unit
            override fun checkServerTrusted(certs: Array<X509Certificate>, authType: String) = Unit
        })
        init(null, trustAllCerts, SecureRandom())
    }.socketFactoryf

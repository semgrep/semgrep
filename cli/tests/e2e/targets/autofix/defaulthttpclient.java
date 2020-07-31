// cf. https://mkyong.com/java/the-type-defaulthttpclient-is-deprecated/

package com.exampleweb.controller;

import org.apache.http.Header;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.DefaultHttpClient;

public class WebCrawler {

	public void crawl(String[] args) throws Exception {
        // ruleid: defaulthttpclient-is-deprecated
		HttpClient client = new DefaultHttpClient();
		HttpGet request = new HttpGet("http://google.com");
		HttpResponse response = client.execute(request);
	}

}

public class SecureWebCrawler {

	public void crawl(String[] args) throws Exception {
        // ok
		HttpClient client = new SystemDefaultHttpClient();
		HttpGet request = new HttpGet("http://google.com");
		HttpResponse response = client.execute(request);
	}

}